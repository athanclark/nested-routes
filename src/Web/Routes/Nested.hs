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
  ( -- * Types
    module X
  , Tries
  , HandlerT (..)
  , ActionT
  , Application'
  -- * Combinators
  , handle
  , parent
  , auth
  , notFound
  -- * Entry Point
  , route
  -- * Extraction
  , extractAuthSym
  -- * Utilities
  , actionToResponse
  , lookupResponse
  , handleUpload
  , plain404
  , lookupProper
  -- ** File Extensions
  , possibleFileExts
  , trimFileExt
  , getFileExt
  , httpMethodToMSym
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
import qualified Data.Map                          as Map
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe                        (fromMaybe)
import           Data.Witherable
import           Data.Bifunctor
import           Data.List
import           Data.Function.Poly
import           Data.Set.Class                    as Sets hiding (singleton)

import           Control.Arrow
import           Control.Error.Util
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer


type Tries x s = ( RUPTrie T.Text x
                 , RUPTrie T.Text x
                 , RUPTrie T.Text s
                 , RUPTrie T.Text x
                 )

newtype HandlerT x s m a = HandlerT
  { runHandlerT :: WriterT (Tries x s) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadTrans
           , MonadWriter (Tries x s)
           )

execHandlerT :: Monad m => HandlerT x s m a -> m (Tries x s)
execHandlerT = execWriterT . runHandlerT

type ActionT m a = VerbListenerT (FileExtListenerT Response m a) m a

-- | For routes ending with a literal.
handle :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , HasResult childContent (ActionT m ())
          , ExpectArity cleanxs childContent
          , Singleton (UrlChunks xs)
              childContent
              (RUPTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childContent)
              (RUPTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childSec)
              (RUPTrie T.Text resultSec)
          , (ArityMinusTypeList childContent cleanxs) ~ resultContent
          , childContent ~ TypeListToArity cleanxs resultContent
          ) => UrlChunks xs -- ^ Path to match against
            -> Maybe childContent -- ^ Possibly a function, ending in @ActionT z m ()@.
            -> Maybe (HandlerT childContent childSec m ()) -- ^ Potential child routes
            -> HandlerT resultContent resultSec m ()
handle ts (Just vl) Nothing = tell (singleton ts vl, mempty, mempty, mempty)
handle ts mvl (Just cs) = do
  (Rooted _ trieContent,_,Rooted _ trieSec,Rooted _ trie401) <- lift $ execHandlerT cs
  tell ( extrude ts $ Rooted mvl trieContent
       , mempty
       , extrude ts $ Rooted Nothing trieSec
       , extrude ts $ Rooted Nothing trie401
       )
handle _ Nothing Nothing = return ()

parent :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , Singleton (UrlChunks xs)
              childContent
              (RUPTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childContent)
              (RUPTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RUPTrie T.Text childSec)
              (RUPTrie T.Text resultSec)
          , (ArityMinusTypeList childContent cleanxs) ~ resultContent
          , childContent ~ TypeListToArity cleanxs resultContent
          ) => UrlChunks xs
            -> HandlerT childContent childSec m ()
            -> HandlerT resultContent resultSec m ()
parent ts cs = do
  (Rooted _ trieContent,_,Rooted _ trieSec,Rooted _ trie401) <- lift $ execHandlerT cs
  tell ( extrude ts $ Rooted Nothing trieContent
       , mempty
       , extrude ts $ Rooted Nothing trieSec
       , extrude ts $ Rooted Nothing trie401
       )


auth :: ( Monad m
        , Functor m
        ) => m sec
          -> content
          -> HandlerT content sec m ()
          -> HandlerT content sec m ()
auth p authFail cs = do
  s <- lift p
  (_,_,Rooted _ trieSec,Rooted _ trie401) <- lift $ execHandlerT cs
  tell ( mempty
       , mempty
       , Rooted (Just s) trieSec
       , Rooted (Just authFail) trie401
       )


notFound :: ( Monad m
            , Functor m
            , cleanxs ~ CatMaybes xs
            , HasResult childContent (ActionT m ())
            , ExpectArity cleanxs childContent
            , Singleton (UrlChunks xs)
                childContent
                (RUPTrie T.Text resultContent)
            , Extrude (UrlChunks xs)
                (RUPTrie T.Text childContent)
                (RUPTrie T.Text resultContent)
            , (ArityMinusTypeList childContent cleanxs) ~ resultContent
            , childContent ~ TypeListToArity cleanxs resultContent
            ) => UrlChunks xs
              -> Maybe childContent
              -> Maybe (HandlerT childContent s m ())
              -> HandlerT resultContent s m ()
notFound ts (Just vl) Nothing = tell (mempty, singleton ts vl, mempty, mempty)
notFound ts mvl (Just cs) = do
  (Rooted _ ctrie,_,_,_) <- lift $ execHandlerT cs
  tell ( mempty
       , extrude ts $ Rooted mvl ctrie
       , mempty
       , mempty
       )
notFound _ Nothing Nothing = return ()




type Application' m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

-- | Turns a @HandlerT@ into a Wai @Application@
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) => HandlerT (ActionT m ()) sec m a -- ^ Assembled @handle@ calls
           -> Application' m
route h req respond = do
  (rtrie, nftrie,_,_) <- execHandlerT h
  let mNotFound = P.lookupNearestParent (pathInfo req) nftrie
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req

  notFoundBasic <- actionToResponse acceptBS Html GET mNotFound plain404 req

  mResp <- runMaybeT $ do
    v <- hoistMaybe $ httpMethodToMSym $ requestMethod req
    failResp <- lift $ actionToResponse acceptBS fe v mNotFound plain404 req

    -- only runs `trimFileExt` when last lookup cell is a Literal
    return $ case P.lookupWithL trimFileExt (pathInfo req) rtrie of
      Nothing -> fromMaybe (liftIO $ respond failResp) $ do
        guard $ not $ null $ pathInfo req
        guard $ trimFileExt (last $ pathInfo req) == "index"
        foundM <- P.lookup (init $ pathInfo req) rtrie
        return $ lookupResponse acceptBS fe v foundM failResp req respond
      Just foundM -> lookupResponse acceptBS fe v foundM failResp req respond

  fromMaybe (liftIO $ respond notFoundBasic) mResp


extractAuthSym :: (Monad m) => HandlerT (ActionT m ()) sec m a -> Request -> m [sec]
extractAuthSym hs req = do
  (_,_,strie,_) <- execHandlerT hs
  return $ P.lookupThrough (pathInfo req) strie

-- (\r -> or <$> runReaderT (extractAuthSym routes r) ) :: Request -> m [Bool]


extractAuthResp :: ( Monad m
                   , MonadIO m
                   ) => HandlerT (ActionT m ()) sec m a -> Application' m
extractAuthResp hs req respond = do
  (_,_,_,srtrie) <- execHandlerT hs
  let mAuthFail = P.lookupNearestParent (pathInfo req) srtrie
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req

  authBasic <- actionToResponse acceptBS Html GET mAuthFail plain401 req

  mResp <- runMaybeT $ do
    v <- hoistMaybe $ httpMethodToMSym $ requestMethod req
    authResp <- lift $ actionToResponse acceptBS fe v mAuthFail plain401 req

    -- only runs `trimFileExt` when last lookup cell is a Literal
    return $ case P.lookupWithL trimFileExt (pathInfo req) srtrie of
      Nothing -> fromMaybe (liftIO $ respond authResp) $ do
        guard $ not $ null $ pathInfo req
        guard $ trimFileExt (last $ pathInfo req) == "index"
        foundM <- P.lookup (init $ pathInfo req) srtrie
        return $ lookupResponse acceptBS fe v foundM authResp req respond
      Just foundM -> lookupResponse acceptBS fe v foundM authResp req respond

  fromMaybe (liftIO $ respond authBasic) mResp




actionToResponse :: MonadIO m =>
                    Maybe B.ByteString
                 -> FileExt
                 -> Verb
                 -> Maybe (ActionT m ()) -- Potential results of 404 lookup
                 -> Response -- Default @404@
                 -> Request
                 -> m Response
actionToResponse acceptBS f v mnfcomp def req = do
  mx <- runMaybeT $ do
    nfcomp <- hoistMaybe mnfcomp
    vmapLit <- lift $ execWriterT $ runVerbListenerT nfcomp
    (_,femonad) <- hoistMaybe $ Map.lookup v $ supplyReq req $ unVerbs $ unUnion vmapLit
    femap <- lift $ execWriterT $ runFileExtListenerT femonad
    hoistMaybe $ lookupProper acceptBS f $ unFileExts femap
  return $ fromMaybe def mx

-- | Turn an @ActionT@ into an @Application@ by providing a @FileExt@ and @Verb@
-- to lookup.
lookupResponse :: MonadIO m =>
                  Maybe B.ByteString -- @Accept@ header
               -> FileExt
               -> Verb
               -> ActionT m ()
               -> Response -- @404@ response
               -> Application' m
lookupResponse acceptBS f v foundM failResp req respond = do
  vmapLit <- execWriterT $ runVerbListenerT foundM
  continue $ supplyReq req $ unVerbs $ unUnion vmapLit
  where
    continue :: MonadIO m =>
                Map.Map Verb ( HandleUpload m
                             , FileExtListenerT Response m ()
                             )
             -> m ResponseReceived
    continue vmap = do
      mResp <- runMaybeT $ do
        (mreqbodyf, femonad) <- hoistMaybe $ Map.lookup v vmap
        femap <- lift $ execWriterT $ runFileExtListenerT femonad
        r <- hoistMaybe $ lookupProper acceptBS f $ unFileExts femap
        case mreqbodyf of
          Nothing              -> return $ liftIO $ respond r
          Just (reqbf,Nothing) -> return $ handleUpload r reqbf req respond
          Just (reqbf,Just bl) -> case requestBodyLength req of
            KnownLength bl' | bl' <= bl -> return $ handleUpload r reqbf req respond
            _                           -> mzero
      fromMaybe (liftIO $ respond failResp) mResp


-- | Terminate the request/response cycle by first handling request body data
-- before responding.
handleUpload :: ( Monad m
                , MonadIO m
                , Functor m
                ) => Response -- ^ Response to send
                  -> (BL.ByteString -> m ()) -- ^ Handle the upload content
                  -> Application' m
handleUpload r' reqbf req' respond' = do
  body <- liftIO $ strictRequestBody req'
  _    <- reqbf body
  liftIO $ respond' r'

-- | Default @404@ response
plain404 :: Response
plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"

-- | Default @401@ response
plain401 :: Response
plain401 = responseLBS status401 [("Content-Type","text/plain")] "401"

-- | Given a possible @Accept@ header and file extension key, lookup the contents
-- of a map.
lookupProper :: Maybe B.ByteString -> FileExt -> Map.Map FileExt a -> Maybe a
lookupProper maccept k xs =
  let attempts = maybe [Html,Text,Json,JavaScript,Css]
                   (possibleFileExts k) maccept
  in foldr (go xs) Nothing attempts
  where
    go xs' x Nothing = Map.lookup x xs'
    go _ _ (Just y) = Just y


-- | Takes a subject file extension and an @Accept@ header, and returns the other
-- types of file types handleable, in order of prescedence.
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
  where
    sortFE Html       xs = [Html, Text]             `intersect` xs
    sortFE JavaScript xs = [JavaScript, Text]       `intersect` xs
    sortFE Json       xs = [Json, JavaScript, Text] `intersect` xs
    sortFE Css        xs = [Css, Text]              `intersect` xs
    sortFE Text       xs = [Text]                   `intersect` xs

-- | Removes @.txt@ from @foo.txt@
trimFileExt :: T.Text -> T.Text
trimFileExt s = if endsWithAny (T.unpack s)
                then T.pack $ takeWhile (/= '.') $ T.unpack s
                else s
  where
    possibleExts = [ ".html",".htm",".txt",".json",".lucid"
                   , ".julius",".css",".cassius",".lucius"
                   ]
    endsWithAny s' = dropWhile (/= '.') s' `Prelude.elem` possibleExts


getFileExt :: Request -> FileExt
getFileExt req = fromMaybe Html $ case pathInfo req of
  [] -> Just Html -- TODO: Override default file extension for `/foo/bar`
  xs -> toExt $ T.dropWhile (/= '.') $ last xs


-- | Turns a @ByteString@ into a @StdMethod@.
httpMethodToMSym :: Method -> Maybe Verb
httpMethodToMSym x | x == methodGet    = Just GET
                   | x == methodPost   = Just POST
                   | x == methodPut    = Just PUT
                   | x == methodDelete = Just DELETE
                   | otherwise         = Nothing


-- * Utilities

onJustM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
onJustM = maybe (return Nothing)
