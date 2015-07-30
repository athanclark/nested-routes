{-# LANGUAGE
    DeriveFunctor
  , GADTs
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , TypeOperators
  , OverloadedStrings
  , TupleSections
  , FlexibleContexts
  , TypeFamilies
  , PolyKinds
  , UndecidableInstances
  #-}

module Web.Routes.Nested
  ( module X
  , HandlerT (..)
  , ActionT
  , handle
  , parent
  , notFound
  , route
  ) where

import           Web.Routes.Nested.Types as X
import           Web.Routes.Nested.FileExtListener as X
import           Web.Routes.Nested.FileExtListener.Types as X
import           Web.Routes.Nested.VerbListener as X

import           Network.HTTP.Types
import           Network.HTTP.Media
import           Network.Wai

import           Data.Trie.Pred.Unified
import qualified Data.Trie.Pred.Unified            as P
import qualified Data.Text                         as T
import qualified Data.Map.Lazy                     as M
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe                        (fromMaybe)
import           Data.Constraint
import           Data.Witherable
import           Data.List
import           Data.Function.Poly

import           Control.Arrow
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer


newtype HandlerT x m a = HandlerT
  { runHandler :: WriterT ( RUPTrie T.Text x
                          , RUPTrie T.Text x ) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type ActionT m a = VerbListenerT (FileExtListenerT Response m a) m a

-- | For routes ending with a literal.
handle :: ( Monad m
          , Functor m
          , cleanxs ~ OnlyJusts xs
          , HasResult childType (ActionT m ())
          , ExpectArity cleanxs childType
          , Singleton (UrlChunks xs)
              childType
              (RUPTrie T.Text result)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childType)
              (RUPTrie T.Text result)
          , (ArityMinusTypeList childType cleanxs) ~ result
          , childType ~ TypeListToArity cleanxs result
          ) => UrlChunks xs -- ^ Path to match against
            -> Maybe childType -- ^ Possibly a function, ending in @ActionT z m ()@.
            -> Maybe (HandlerT childType m ()) -- ^ Potential child routes
            -> HandlerT result m ()
handle ts (Just vl) Nothing =
  HandlerT $ tell (singleton ts vl, mempty)
handle ts mvl (Just cs) = do
  (Rooted _ ctrie,_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (extrude ts $ Rooted mvl ctrie, mempty)
handle _ Nothing Nothing = return ()

parent :: ( Monad m
          , Functor m
          , cleanxs ~ OnlyJusts xs
          , Singleton (UrlChunks xs)
              childType
              (RUPTrie T.Text result)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childType)
              (RUPTrie T.Text result)
          , (ArityMinusTypeList childType cleanxs) ~ result
          , childType ~ TypeListToArity cleanxs result
          ) => UrlChunks xs
            -> HandlerT childType m ()
            -> HandlerT result m ()
parent ts cs = do
  (Rooted _ ctrie,_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (extrude ts $ Rooted Nothing ctrie, mempty)

notFound :: ( Monad m
            , Functor m
            , cleanxs ~ OnlyJusts xs
            , HasResult childType (ActionT m ())
            , ExpectArity cleanxs childType
            , Singleton (UrlChunks xs)
                childType
                (RUPTrie T.Text result)
            , Extrude (UrlChunks xs)
                (RUPTrie T.Text childType)
                (RUPTrie T.Text result)
            , (ArityMinusTypeList childType cleanxs) ~ result
            , childType ~ TypeListToArity cleanxs result
            ) => UrlChunks xs
              -> Maybe childType
              -> Maybe (HandlerT childType m ())
              -> HandlerT result m ()
notFound ts (Just vl) Nothing =
  HandlerT $ tell (mempty, singleton ts vl)
notFound ts mvl (Just cs) = do
  (Rooted _ ctrie,_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (mempty, extrude ts $ Rooted mvl ctrie)
notFound _ Nothing Nothing = return ()


type Middleware' m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

-- | Turns a @HandlerT@ into a Wai @Application@
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) => HandlerT (ActionT m ()) m a -- ^ Assembled @handle@ calls
           -> Middleware' m
route h req respond = do
  (rtrie, nftrie) <- execWriterT $ runHandler h
  let mMethod  = httpMethodToMSym $ requestMethod req
      mFileext = case pathInfo req of
                         [] -> Just Html
                         xs -> toExt $ T.dropWhile (/= '.') $ last xs
      mnftrans = P.lookupNearestParent (pathInfo req) nftrie
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = fromMaybe Html mFileext

  notFoundBasic <- handleNotFound req acceptBS Html GET mnftrans

  case mMethod of
    Nothing -> liftIO $ respond404 notFoundBasic
    Just v  -> do
      menf <- handleNotFound req acceptBS fe v mnftrans
      let failResp = liftIO $ respond404 menf

      case P.lookupWithL trimFileExt (pathInfo req) rtrie of
        Nothing -> case pathInfo req of
          [] -> failResp
          _  -> case trimFileExt $ last $ pathInfo req of
                  "index" -> maybe failResp
                               (\foundM -> continue req acceptBS fe v foundM menf) $
                               P.lookup (init $ pathInfo req) rtrie
                  _ -> failResp
        Just foundM -> continue req acceptBS fe v foundM menf

  where
    onJustM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
    onJustM = maybe (return Nothing)


    handleNotFound :: MonadIO m =>
                      Request
                   -> Maybe B.ByteString
                   -> FileExt
                   -> Verb
                   -> Maybe (ActionT m ())
                   -> m (Maybe Response)
    handleNotFound req acceptBS f v mnfcomp =
      let handleEither nfcomp = do
            vmapLit <- execWriterT $ runVerbListenerT nfcomp
            onJustM (\(_, femonad) -> do
              femap <- execWriterT $ runFileExtListenerT femonad
              return $ lookupProper acceptBS f $ unFileExts femap) $
                M.lookup v $ supplyReq req $ unVerbs vmapLit
      in onJustM handleEither mnfcomp


    continue :: MonadIO m =>
                Request
             -> Maybe B.ByteString
             -> FileExt
             -> Verb
             -> ActionT m ()
             -> Maybe Response
             -> m ResponseReceived
    continue req acceptBS f v foundM mnfResp = do
      vmapLit <- execWriterT $ runVerbListenerT foundM
      continueMap acceptBS f v (supplyReq req $ unVerbs vmapLit) mnfResp

    continueMap :: MonadIO m =>
                   Maybe B.ByteString
                -> FileExt
                -> Verb
                -> M.Map Verb ( Maybe (BL.ByteString -> m (), Maybe BodyLength)
                              , FileExtListenerT Response m ()
                              )
                -> Maybe Response
                -> m ResponseReceived
    continueMap acceptBS f v vmap mnfResp = do
      let failResp = liftIO $ respond404 mnfResp

      maybe failResp (\(mreqbodyf, femonad) -> do
          femap <- execWriterT $ runFileExtListenerT femonad
          maybe failResp (\r ->
              case mreqbodyf of
                Nothing              -> liftIO $ respond r
                Just (reqbf,Nothing) -> handleUpload req reqbf respond r
                Just (reqbf,Just bl) ->
                  case requestBodyLength req of
                    KnownLength bl' ->
                      if bl' <= bl
                      then handleUpload req reqbf respond r
                      else failResp
                    _ -> failResp) $
            lookupProper acceptBS f $ unFileExts femap) $
        M.lookup v vmap

    handleUpload req reqbf respond r = do
      body <- liftIO $ strictRequestBody req
      reqbf body
      liftIO $ respond r

    respond404 :: Maybe Response -> IO ResponseReceived
    respond404 mr = respond $ fromMaybe plain404 mr

    plain404 :: Response
    plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"

    lookupProper :: Maybe B.ByteString -> FileExt -> M.Map FileExt a -> Maybe a
    lookupProper maccept k xs =
      let attempts = maybe [Html,Text,Json,JavaScript,Css]
                       (possibleFileExts k) maccept
      in foldr (go xs) Nothing attempts
      where
        go xs x Nothing = M.lookup x xs
        go _ _ (Just y) = Just y

    possibleFileExts :: FileExt -> B.ByteString -> [FileExt]
    possibleFileExts fe accept =
      let computed = sortFE fe $ nub $ concat $
            catMaybes [ mapAccept [ ("application/json" :: B.ByteString, [Json])
                                  , ("application/javascript" :: B.ByteString, [Json,JavaScript])
                                  ] accept
                      , mapAccept [ ("text/html" :: B.ByteString, [Html])
                                  ] accept
                      , mapAccept [ ("text/plain" :: B.ByteString, [Text])
                                  ] accept
                      , mapAccept [ ("text/css" :: B.ByteString, [Css])
                                  ] accept
                      ]

          wildcard = concat $
            catMaybes [ mapAccept [ ("*/*" :: B.ByteString, [Html,Text,Json,JavaScript,Css])
                                  ] accept
                      ]
      in if not (null wildcard) then wildcard else computed

    sortFE Html       xs = [Html, Text]             `intersect` xs
    sortFE JavaScript xs = [JavaScript, Text]       `intersect` xs
    sortFE Json       xs = [Json, JavaScript, Text] `intersect` xs
    sortFE Css        xs = [Css, Text]              `intersect` xs
    sortFE Text       xs = [Text]                   `intersect` xs

    trimFileExt :: T.Text -> T.Text
    trimFileExt s = if T.unpack s `endsWithAny` possibleExts
                    then T.pack $ takeWhile (/= '.') $ T.unpack s
                    else s
      where
        possibleExts = [ ".html",".htm",".txt",".json",".lucid"
                       , ".julius",".css",".cassius",".lucius"
                       ]
        endsWithAny s xs = dropWhile (/= '.') s `elem` xs

    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just GET
                       | x == methodPost   = Just POST
                       | x == methodPut    = Just PUT
                       | x == methodDelete = Just DELETE
                       | otherwise         = Nothing
