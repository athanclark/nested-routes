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
  , routeAuth
  -- * Extraction
  , extractContent
  , extractNotFound
  , extractAuthSym
  , extractAuthResp
  , extractNearestVia
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

import           Data.Trie.Pred.Types (RootedPredTrie (..), PredTrie (..))
import qualified Data.Trie.Pred.Types              as PT -- only using lookups
import           Data.Trie.Pred.Step (PredSteps (..), PredStep (..))
import qualified Data.Trie.Class                   as TC
import qualified Data.Text                         as T
import qualified Data.Map                          as Map
import           Data.Trie.Map (MapStep (..))
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as BL
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty                as NE
import           Data.Maybe                        (fromMaybe)
import           Data.Witherable
import           Data.Functor.Syntax
import           Data.Function.Poly
import           Data.List
import           Data.Function.Poly
import           Data.Set.Class                    as Sets hiding (singleton)

import           Control.Arrow
import           Control.Error.Util
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Control.Monad.Except


type Tries x s e e' = ( RootedPredTrie T.Text x
                      , RootedPredTrie T.Text x
                      , RootedPredTrie T.Text s
                      , RootedPredTrie T.Text e
                      )

newtype HandlerT x sec err errSym m a = HandlerT
  { runHandlerT :: WriterT (Tries x sec err errSym) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadTrans
           , MonadWriter (Tries x sec err errSym)
           )

execHandlerT :: Monad m => HandlerT x sec err errSym m a -> m (Tries x sec err errSym)
execHandlerT = execWriterT . runHandlerT

type ActionT m a = VerbListenerT (FileExtListenerT Response m a) m a

-- | For routes ending with a literal.
handle :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , HasResult childContent (ActionT m ())
          , HasResult childErr     (e -> ActionT m ())
          , ExpectArity cleanxs childContent
          , ExpectArity cleanxs childSec
          , ExpectArity cleanxs childErr
          , Singleton (UrlChunks xs)
              childContent
              (RootedPredTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RootedPredTrie T.Text childContent)
              (RootedPredTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RootedPredTrie T.Text childSec)
              (RootedPredTrie T.Text resultSec)
          , Extrude (UrlChunks xs)
              (RootedPredTrie T.Text childErr)
              (RootedPredTrie T.Text resultErr)
          , (ArityMinusTypeList childContent cleanxs) ~ resultContent
          , (ArityMinusTypeList childSec cleanxs) ~ resultSec
          , (ArityMinusTypeList childErr cleanxs) ~ resultErr
          , childContent ~ TypeListToArity cleanxs resultContent
          , childSec ~ TypeListToArity cleanxs resultSec
          , childErr ~ TypeListToArity cleanxs resultErr
          ) => UrlChunks xs -- ^ Path to match against
            -> Maybe childContent -- ^ Possibly a function, ending in @ActionT z m ()@.
            -> Maybe (HandlerT childContent  childSec  childErr  e m ()) -- ^ Potential child routes
            ->        HandlerT resultContent resultSec resultErr e m ()
handle ts (Just vl) Nothing = tell (singleton ts vl, mempty, mempty, mempty)
handle ts mvl (Just cs) = do
  (RootedPredTrie _ trieContent,trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts $ RootedPredTrie mvl trieContent
       , extrude ts trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )
handle _ Nothing Nothing = return ()

parent :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , Singleton (UrlChunks xs)
              childContent
              (RootedPredTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RootedPredTrie T.Text childContent)
              (RootedPredTrie T.Text resultContent)
          , Extrude (UrlChunks xs)
              (RootedPredTrie T.Text childErr)
              (RootedPredTrie T.Text resultErr)
          , Extrude (UrlChunks xs)
              (RootedPredTrie T.Text childSec)
              (RootedPredTrie T.Text resultSec)
          , (ArityMinusTypeList childContent cleanxs) ~ resultContent
          , (ArityMinusTypeList childSec cleanxs) ~ resultSec
          , (ArityMinusTypeList childErr cleanxs) ~ resultErr
          , childContent ~ TypeListToArity cleanxs resultContent
          , childSec ~ TypeListToArity cleanxs resultSec
          , childErr ~ TypeListToArity cleanxs resultErr
          ) => UrlChunks xs
            -> HandlerT childContent  childSec  childErr  e m ()
            -> HandlerT resultContent resultSec resultErr e m ()
parent ts cs = do
  (trieContent,trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts trieContent
       , extrude ts trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )


-- | Sets the security role and error handler for a scope of routes.
auth :: ( Monad m
        , Functor m
        ) => sec
          -> err
          -> HandlerT content sec err e m ()
auth s handleFail =
  tell ( mempty
       , mempty
       , RootedPredTrie (Just s) mempty
       , RootedPredTrie (Just handleFail) mempty
       )


notFound :: ( Monad m
            , Functor m
            , cleanxs ~ CatMaybes xs
            , HasResult childContent (ActionT m ())
            , HasResult childErr     (e -> ActionT m ())
            , ExpectArity cleanxs childContent
            , ExpectArity cleanxs childSec
            , ExpectArity cleanxs childErr
            , Singleton (UrlChunks xs)
                childContent
                (RootedPredTrie T.Text resultContent)
            , Extrude (UrlChunks xs)
                (RootedPredTrie T.Text childContent)
                (RootedPredTrie T.Text resultContent)
            , Extrude (UrlChunks xs)
                (RootedPredTrie T.Text childSec)
                (RootedPredTrie T.Text resultSec)
            , Extrude (UrlChunks xs)
                (RootedPredTrie T.Text childErr)
                (RootedPredTrie T.Text resultErr)
            , (ArityMinusTypeList childContent cleanxs) ~ resultContent
            , (ArityMinusTypeList childSec cleanxs) ~ resultSec
            , (ArityMinusTypeList childErr cleanxs) ~ resultErr
            , childContent ~ TypeListToArity cleanxs resultContent
            , childSec ~ TypeListToArity cleanxs resultSec
            , childErr ~ TypeListToArity cleanxs resultErr
            ) => UrlChunks xs
              -> Maybe childContent
              -> Maybe (HandlerT childContent  childSec  childErr  e m ())
              ->        HandlerT resultContent resultSec resultErr e m ()
notFound ts (Just vl) Nothing = tell (mempty, singleton ts vl, mempty, mempty)
notFound ts mvl (Just cs) = do
  (trieContent,RootedPredTrie _ trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts trieContent
       , extrude ts $ RootedPredTrie mvl trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )
notFound _ Nothing Nothing = return ()




type Application' m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
type Middleware' m = Application' m -> Application' m


-- | Turns a @HandlerT@ into a Wai @Application@
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) => HandlerT (ActionT m ()) sec err e m a -- ^ Assembled @handle@ calls
           -> Application' m
route = extractContent


-- | Given a security verification function, and a security updating function,
-- turn a set of routes into a final application, where content is secured before
-- being breached.
routeAuth :: ( Functor m
             , Monad m
             , MonadIO m
             ) => (Request -> [sec] -> ExceptT e m checksum) -- ^ create
               -> (checksum -> m (Response -> Response))     -- ^ set
               -> HandlerT (ActionT m ()) sec (e -> ActionT m ()) e m a -- ^ Assembled @handle@ calls
               -> Application' m
routeAuth makeAuth putAuth hs req respond = do
  ss <- extractAuthSym hs req
  eNewData <- runExceptT $ makeAuth req ss
  case eNewData of
    Left  e       -> extractAuthResp e hs req respond
    Right newData -> do
      f <- putAuth newData
      extractContent hs req $ respond . f


-- | Compress the content tries (normal and not-found responses) into a final
-- application.
extractContent :: ( Functor m
                  , Monad m
                  , MonadIO m
                  ) => HandlerT (ActionT m ()) sec err e m a -- ^ Assembled @handle@ calls
                    -> Application' m
extractContent h req respond = do
  (rtrie, nftrie,_,_) <- execHandlerT h
  let mNotFound = getResultsFromMatch <$> PT.matchRPT (pathInfo req) nftrie
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req

  notFoundBasic <- actionToResponse acceptBS Html GET mNotFound plain404 req

  mResp <- runMaybeT $ do
    v <- hoistMaybe $ httpMethodToMSym $ requestMethod req
    failResp <- lift $ actionToResponse acceptBS fe v mNotFound plain404 req

    -- only runs `trimFileExt` when last lookup cell is a Literal
    return $ case lookupWithLRPT trimFileExt (pathInfo req) rtrie of
      Nothing -> fromMaybe (liftIO $ respond failResp) $ do
        guard $ not $ null $ pathInfo req
        guard $ trimFileExt (last $ pathInfo req) == "index"
        foundM <- TC.lookup (init $ pathInfo req) rtrie
        return $ lookupResponse acceptBS fe v foundM failResp req respond
      Just foundM -> lookupResponse acceptBS fe v foundM failResp req respond

  fromMaybe (liftIO $ respond notFoundBasic) mResp


-- | Manually fetch the security tokens / authorization roles affiliated with
-- a request and your routing system.
extractAuthSym :: ( Functor m
                  , Monad m
                  ) => HandlerT x sec err e m a
                    -> Request
                    -> m [sec]
extractAuthSym hs req = do
  (_,_,trie,_) <- execHandlerT hs
  return $ getResultsFromMatch <$> PT.matchesRPT (pathInfo req) trie


extractAuthResp :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => e -- TODO: Mirror `routeAuth`
                     -> HandlerT x sec (e -> ActionT m ()) e m a
                     -> Application' m
extractAuthResp e hs req respond = do
  (_,_,_,trie) <- execHandlerT hs
  let mAction = (getResultsFromMatch <$> PT.matchRPT (pathInfo req) trie) <$~> e
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req
  basicResp <- actionToResponse acceptBS Html GET mAction plain401 req
  mResp <- runMaybeT $ do
    v <- hoistMaybe $ httpMethodToMSym $ requestMethod req
    lift $ actionToResponse acceptBS fe v mAction plain401 req
  liftIO $ respond $ fromMaybe basicResp mResp


-- | Turns the not-found trie into a final application, matching all routes under
-- each @notFound@ node.
extractNotFound :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => HandlerT (ActionT m ()) sec err e m a
                     -> Application' m
extractNotFound = extractNearestVia (execHandlerT >=> \(_,t,_,_) -> return t) plain404


extractNearestVia :: ( Functor m
                     , Monad m
                     , MonadIO m
                     ) => (HandlerT (ActionT m ()) sec err e m a -> m (RootedPredTrie T.Text (ActionT m ())))
                       -> Response -- ^ Default
                       -> HandlerT (ActionT m ()) sec err e m a
                       -> Application' m
extractNearestVia extr def hs req respond = do
  trie <- extr hs
  let mAction = getResultsFromMatch <$> PT.matchRPT (pathInfo req) trie
      acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req
  basicResp <- actionToResponse acceptBS Html GET mAction def req
  mResp <- runMaybeT $ do
    v <- hoistMaybe $ httpMethodToMSym $ requestMethod req
    lift $ actionToResponse acceptBS fe v mAction def req
  liftIO $ respond $ fromMaybe basicResp mResp


actionToResponse :: MonadIO m =>
                    Maybe B.ByteString
                 -> FileExt
                 -> Verb
                 -> Maybe (ActionT m ()) -- ^ Potential results to lookup
                 -> Response -- ^ Default Response
                 -> Request
                 -> m Response
actionToResponse acceptBS f v mAction def req = do
  mx <- runMaybeT $ do
    action <- hoistMaybe mAction
    vmap <- lift $ execWriterT $ runVerbListenerT action
    (_,fexts) <- hoistMaybe $ Map.lookup v $ supplyReq req $ unVerbs $ unUnion vmap
    femap <- lift $ execWriterT $ runFileExtListenerT fexts
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


----- Internal Utilities -----------------------------------

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


-- * Pred-Trie related -----------------

-- | A quirky function for processing the last element of a lookup path, only
-- on /literal/ matches.
lookupWithLPT :: Ord s => (s -> s) -> NonEmpty s -> PredTrie s a -> Maybe a
lookupWithLPT f (t:|ts) (PredTrie (MapStep ls) (PredSteps ps))
  | null ts = getFirst $ First (goLit (f t) ls) <> foldMap (First . goPred) ps
  | otherwise = getFirst $ First (goLit t ls) <> foldMap (First . goPred) ps
  where
    goLit t' xs = do (mx,mxs) <- Map.lookup t' xs
                     if null ts then mx
                                else lookupWithLPT f (NE.fromList ts) =<< mxs

    goPred (PredStep _ p mx xs) = do
      r <- p t
      if null ts then mx <$~> r
                 else lookupWithLPT f (NE.fromList ts) xs <$~> r

lookupWithLRPT :: Ord s => (s -> s) -> [s] -> RootedPredTrie s a -> Maybe a
lookupWithLRPT _ [] (RootedPredTrie mx _) = mx
lookupWithLRPT f ts (RootedPredTrie _ xs) = lookupWithLPT f (NE.fromList ts) xs

getResultsFromMatch :: ([s],a,[s]) -> a
getResultsFromMatch (_,x,_) = x
