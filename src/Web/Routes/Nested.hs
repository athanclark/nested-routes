{-# LANGUAGE
    DeriveFunctor
  , GADTs
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeOperators
  , OverloadedStrings
  , DataKinds
  , TupleSections
  , FlexibleContexts
  , ConstraintKinds
  , DataKinds
  , KindSignatures
  , TypeFamilies
  , RankNTypes
  , PolyKinds
  , UndecidableInstances
  #-}

module Web.Routes.Nested
  ( module Web.Routes.Nested.FileExtListener
  , module Web.Routes.Nested.VerbListener
  , module Web.Routes.Nested.Types
  , HandlerT (..)
  , ActionT
  , handle
  , notFound
  , route
  ) where

import           Web.Routes.Nested.Types
import           Web.Routes.Nested.FileExtListener
import           Web.Routes.Nested.FileExtListener.Types (FileExt (..))
import           Web.Routes.Nested.VerbListener

import           Network.HTTP.Types
import           Network.HTTP.Media
import           Network.Wai

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Monad.Reader
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

import Data.Function.Poly


newtype HandlerT z x m a = HandlerT
  { runHandler :: WriterT ( RUPTrie T.Text x
                          , RUPTrie T.Text x ) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z x m)
deriving instance Monad m =>       Monad       (HandlerT z x m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z x m)
instance MonadTrans (HandlerT z x) where
  lift ma = HandlerT $ lift ma

type ActionT z m a = VerbListenerT z (FileExtListenerT Response m a) m a

type family LastIsNothing (xs :: [Maybe *]) :: Constraint where
  LastIsNothing '[] = ()
  LastIsNothing ('Nothing ': '[]) = ()
  LastIsNothing (x ': xs) = LastIsNothing xs

type family LastIsJust (xs :: [Maybe *]) :: Constraint where
  LastIsJust (('Just x) ': '[]) = ()
  LastIsJust (x ': xs) = LastIsJust xs

-- | For routes ending with a literal.
handle :: ( Monad m
          , Functor m
          , cleanxs ~ OnlyJusts xs
          , HasResult childType (ActionT z m ())
          , ExpectArity cleanxs childType
          , Singleton (UrlChunks xs)
              childType
              (RUPTrie T.Text result)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childType)
              (RUPTrie T.Text result)
          , (ArityMinusTypeList childType cleanxs) ~ result
          , childType ~ TypeListToArity cleanxs result
          , LastIsNothing xs
          ) =>
          UrlChunks xs -- ^ Path to match against
       -> Maybe childType -- ^ Possibly a function, ending in @ActionT z m ()@.
       -> Maybe (HandlerT z childType m ()) -- ^ Potential child routes
       -> HandlerT z result m ()
handle ts (Just vl) Nothing =
  HandlerT $ tell (singleton ts vl, mempty)
handle ts mvl (Just cs) = do
  ((Rooted _ ctrie),_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (extrude ts $ Rooted mvl ctrie, mempty)
handle _ Nothing Nothing = return ()


notFound :: ( Monad m
            , Functor m
            , cleanxs ~ OnlyJusts xs
            , HasResult childType (ActionT z m ())
            , ExpectArity cleanxs childType
            , Singleton (UrlChunks xs)
                childType
                (RUPTrie T.Text result)
            , Extrude (UrlChunks xs)
                (RUPTrie T.Text childType)
                (RUPTrie T.Text result)
            , (ArityMinusTypeList childType cleanxs) ~ result
            , childType ~ TypeListToArity cleanxs result
            ) =>
            UrlChunks xs
         -> Maybe childType
         -> Maybe (HandlerT z childType m ())
         -> HandlerT z result m ()
notFound ts (Just vl) Nothing = do
  HandlerT $ tell (mempty, singleton ts vl)
notFound ts mvl (Just cs) = do
  ((Rooted _ ctrie),_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (mempty, extrude ts $ Rooted mvl ctrie)
notFound _ Nothing Nothing = return ()


-- | Turns a @HandlerT@ into a Wai @Application@
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) =>
         HandlerT z (ActionT z m ()) m a -- ^ Assembled @handle@ calls
      -> Request
      -> (Response -> IO ResponseReceived) -> m ResponseReceived
route h req respond = do
  (rtrie, nftrie) <- execWriterT $ runHandler h
  let mMethod  = httpMethodToMSym $ requestMethod req
      mFileext = case pathInfo req of
                         [] -> Just Html
                         xs -> toExt $ T.pack $ dropWhile (/= '.') $ T.unpack $ last xs
      mnftrans = P.lookupNearestParent (pathInfo req) nftrie
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = fromMaybe Html mFileext

  notFoundBasic <- handleNotFound acceptBS Html Get mnftrans

  case mMethod of
    Nothing -> liftIO $ respond404 notFoundBasic
    Just v  -> do
      menf <- handleNotFound acceptBS fe v mnftrans
      let cleanedPathInfo = applyToLast trimFileExt $ pathInfo req
          fail = liftIO $ respond404 menf

      case P.lookup cleanedPathInfo rtrie of
        Nothing -> case pathInfo req of
          [] -> fail
          _  -> case trimFileExt $ last $ pathInfo req of
                  "index" -> maybe fail
                               (\foundM -> continue acceptBS fe v foundM menf) $
                               P.lookup (init $ pathInfo req) rtrie
                  _ -> fail
        Just foundM -> continue acceptBS fe v foundM menf

  where
    onJustM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
    onJustM f mx = maybe (return Nothing) f mx


    handleNotFound :: MonadIO m =>
                      Maybe B.ByteString
                   -> FileExt
                   -> Verb
                   -> Maybe (ActionT z m ())
                   -> m (Maybe Response)
    handleNotFound acceptBS f v mnfcomp =
      let handleEither nfcomp = do
            vmapLit <- execWriterT $ runVerbListenerT nfcomp
            onJustM (\(_, femonad) -> do
              femap <- execWriterT $ runFileExtListenerT femonad
              return $ lookupProper acceptBS f $ unFileExts femap) $
                M.lookup v $ unVerbs vmapLit
      in
      onJustM handleEither mnfcomp


    continue :: MonadIO m =>
                Maybe B.ByteString
             -> FileExt
             -> Verb
             -> ActionT z m ()
             -> Maybe Response
             -> m ResponseReceived
    continue acceptBS f v foundM mnfResp = do
      vmapLit <- execWriterT $ runVerbListenerT foundM
      continueMap acceptBS f v (unVerbs vmapLit) mnfResp

    continueMap :: MonadIO m =>
                   Maybe B.ByteString
                -> FileExt
                -> Verb
                -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), FileExtListenerT Response m ())
                -> Maybe Response
                -> m ResponseReceived
    continueMap acceptBS f v vmap mnfResp = do
      let fail = liftIO $ respond404 mnfResp

      maybe fail (\(mreqbodyf, femonad) -> do
          femap <- execWriterT $ runFileExtListenerT femonad
          maybe fail (\r -> do
              case mreqbodyf of
                Nothing              -> liftIO $ respond r
                Just (reqbf,Nothing) -> do
                  body <- liftIO $ strictRequestBody req
                  (runReaderT $ reqbf) body
                  liftIO $ respond r
                Just (reqbf,Just bl) -> do
                  case requestBodyLength req of
                    KnownLength bl' ->
                      if bl' <= bl
                      then do body <- liftIO $ strictRequestBody req
                              (runReaderT $ reqbf) body
                              liftIO $ respond r
                      else fail
                    _ -> fail) $
            lookupProper acceptBS f $ unFileExts femap) $
        M.lookup v vmap


    respond404 :: Maybe Response -> IO ResponseReceived
    respond404 mr = respond $ fromMaybe plain404 mr

    plain404 :: Response
    plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"

    lookupProper :: Maybe B.ByteString -> FileExt -> M.Map FileExt a -> Maybe a
    lookupProper maccept k map =
      let attempts = maybe
                       [Html,Text,Json,JavaScript,Css]
                       (\accept -> possibleFileExts k accept)
                       maccept
      in
      foldr (go map) Nothing attempts
      where
        go map x Nothing = M.lookup x map
        go _ _  (Just y) = Just y

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
      in
      if length wildcard /= 0 then wildcard else computed

    sortFE Html       xs = [Html, Text]             `intersect` xs
    sortFE JavaScript xs = [JavaScript, Text]       `intersect` xs
    sortFE Json       xs = [Json, JavaScript, Text] `intersect` xs
    sortFE Css        xs = [Css, Text]              `intersect` xs
    sortFE Text       xs = [Text]                   `intersect` xs

    applyToLast :: (a -> a) -> [a] -> [a]
    applyToLast _ [] = []
    applyToLast f (x:[]) = f x : []
    applyToLast f (x:xs) = x : applyToLast f xs

    trimFileExt :: T.Text -> T.Text
    trimFileExt s = if (T.unpack s) `endsWithAny` possibleExts
                    then T.pack $ takeWhile (/= '.') $ T.unpack s
                    else s
      where
        possibleExts = [ ".html",".htm",".txt",".json",".lucid"
                       , ".julius",".css",".cassius",".lucius"
                       ]
        endsWithAny s xs = (dropWhile (/= '.') s) `elem` xs

    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just Get
                       | x == methodPost   = Just Post
                       | x == methodPut    = Just Put
                       | x == methodDelete = Just Delete
                       | otherwise         = Nothing
