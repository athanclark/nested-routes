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
  , ApplicationT
  , MiddlewareT
  , AuthScope (..)
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
  , extractAuth
  , extractNearestVia
  -- * Utilities
  , actionToMiddleware
  , lookupVerb
  , lookupFileExt
  , lookupResponse
  , handleUpload
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

import           Data.Trie.Pred (RootedPredTrie (..), PredTrie (..))
import qualified Data.Trie.Pred                    as PT -- only using lookups
import           Data.Trie.Pred.Step (PredSteps (..), PredStep (..))
import qualified Data.Trie.Class                   as TC
import qualified Data.Text                         as T
import qualified Data.Map                          as Map
import           Data.Trie.Map (MapStep (..))
import qualified Data.ByteString                   as B
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty                as NE
import           Data.Maybe                        (fromMaybe)
import           Data.Witherable hiding (filter)
import           Data.Functor.Syntax
import           Data.Function.Poly
import           Data.List hiding (filter)

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
          ) => UrlChunks xs
            -> Maybe childContent
            -> Maybe (HandlerT childContent  childSec  childErr  e m ())
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


data AuthScope = ProtectParent | ProtectChildren
  deriving (Show, Eq)

-- | Sets the security role and error handler for a scope of routes.
auth :: ( Monad m
        , Functor m
        ) => sec
          -> err
          -> AuthScope
          -> HandlerT content (sec, AuthScope) err e m ()
auth token handleFail scope =
  tell ( mempty
       , mempty
       , RootedPredTrie (Just (token,scope)) mempty
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




type ApplicationT m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m

type AcceptHeader = B.ByteString


-- | Turns a @HandlerT@ into a Wai @Application@
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) => HandlerT (ActionT m ()) sec err e m a -- ^ Assembled @handle@ calls
           -> MiddlewareT m
route hs = extractContent hs . extractNotFound hs


-- | Given a security verification function which returns an updating function,
-- turn a set of routes into a middleware, where a session is secured before
-- responding.
routeAuth :: ( Functor m
             , Monad m
             , MonadIO m
             ) => (Request -> [sec] -> ExceptT e m (Response -> Response)) -- ^ authorize
               -> HandlerT (ActionT m ()) (sec, AuthScope) (e -> ActionT m ()) e m a -- ^ Assembled @handle@ calls
               -> MiddlewareT m
routeAuth authorize hs = extractAuth authorize hs . route hs

-- | Turn the trie carrying the main content into a middleware.
extractContent :: ( Functor m
                  , Monad m
                  , MonadIO m
                  ) => HandlerT (ActionT m ()) sec err e m a -- ^ Assembled @handle@ calls
                    -> MiddlewareT m
extractContent hs app req respond = do
  (rtrie,_,_,_) <- execHandlerT hs
  let mAcceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req
      v = getVerb req
  case lookupWithLRPT trimFileExt (pathInfo req) rtrie of
    Nothing -> fromMaybe (app req respond) $ do
      guard $ not $ null $ pathInfo req
      guard $ trimFileExt (last $ pathInfo req) == "index"
      found <- TC.lookup (init $ pathInfo req) rtrie
      Just $ actionToMiddleware mAcceptBS fe v found app req respond
    Just found -> actionToMiddleware mAcceptBS fe v found app req respond


-- | Manually fetch the security tokens / authorization roles affiliated with
-- a request and your routing system.
extractAuthSym :: ( Functor m
                  , Monad m
                  ) => HandlerT x (sec, AuthScope) err e m a
                    -> Request
                    -> m [sec]
extractAuthSym hs req = do
  (_,_,trie,_) <- execHandlerT hs
  return $ foldl go [] (PT.matchesRPT (pathInfo req) trie)
  where
    go ys (_,(_,ProtectChildren),[]) = ys
    go ys (pre,(x,_),suff) = ys ++ [x]


extractAuth :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => (Request -> [sec] -> ExceptT e m (Response -> Response)) -- authorization method
                     -> HandlerT x (sec, AuthScope) (e -> ActionT m ()) e m a
                     -> MiddlewareT m
extractAuth authorize hs app req respond = do
  (_,_,_,trie) <- execHandlerT hs
  ss <- extractAuthSym hs req
  ef <- runExceptT $ authorize req ss
  let acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req
      v = getVerb req
  either (\e -> maybe (app req respond)
                      (\action -> actionToMiddleware acceptBS fe v action app req respond)
                    $ (getResultsFromMatch <$> PT.matchRPT (pathInfo req) trie) <$~> e)
         (\f -> app req (respond . f))
         ef

-- | Turns the not-found trie into a final application, matching all routes under
-- each @notFound@ node. If there is no nearest parent (querying above the head
-- of the tree), control is passed down the middlware chain.
extractNotFound :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => HandlerT (ActionT m ()) sec err e m a
                     -> MiddlewareT m
extractNotFound = extractNearestVia (execHandlerT >=> \(_,t,_,_) -> return t)


-- | Only return content, do not handle uploads. Also, the extraction should be
-- flat, in that the values contained in our trie are only @ActionT@, without arity.
extractNearestVia :: ( Functor m
                     , Monad m
                     , MonadIO m
                     ) => (HandlerT (ActionT m ()) sec err e m a -> m (RootedPredTrie T.Text (ActionT m ())))
                       -> HandlerT (ActionT m ()) sec err e m a
                       -> MiddlewareT m
extractNearestVia extr hs app req respond = do
  trie <- extr hs
  let acceptBS = Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req
      fe = getFileExt req
      v = getVerb req
  maybe (app req respond)
        (\action -> actionToMiddleware acceptBS fe v action app req respond)
      $ getResultsFromMatch <$> PT.matchRPT (pathInfo req) trie



-- | Turn an @ActionT@ into a @Middleware@ by providing a @FileExt@ and @Verb@
-- to lookup, returning the response and utilizing the upload handler encoded
-- in the action.
actionToMiddleware :: MonadIO m =>
                      Maybe AcceptHeader -- @Accept@ header
                   -> FileExt
                   -> Verb
                   -> ActionT m ()
                   -> MiddlewareT m
actionToMiddleware mAcceptBS f v found app req respond = do
  mResponse <- lookupResponse mAcceptBS f v req found
  maybe (app req respond) go mResponse
  where go (mreqbodyf,r) = do handleUpload mreqbodyf req
                              liftIO $ respond r


lookupResponse :: MonadIO m =>
                          Maybe AcceptHeader
                       -> FileExt
                       -> Verb
                       -> Request
                       -> ActionT m ()
                       -> m (Maybe (HandleUpload m, Response))
lookupResponse mAcceptBS f v req action = runMaybeT $ do
  vmap <- lift $ execVerbListenerT action
  (mreqbodyf,fexts) <- hoistMaybe $ lookupVerb v req vmap
  femap <- lift $ execFileExtListenerT fexts
  r <- hoistMaybe $ lookupFileExt mAcceptBS f femap
  return (mreqbodyf, r)


handleUpload :: MonadIO m =>
                     HandleUpload m
                  -> Request
                  -> m ()
handleUpload mreqbodyf req = case mreqbodyf of
  Nothing              -> return ()
  Just (reqbf,Nothing) -> go reqbf
  Just (reqbf,Just bl) -> case requestBodyLength req of
    KnownLength bl' | bl' <= bl -> go reqbf
    _                           -> return ()
  where go reqbf = reqbf =<< liftIO (strictRequestBody req)



lookupVerb :: Verb -> Request -> Verbs m r -> Maybe (HandleUpload m, r)
lookupVerb v req vmap = Map.lookup v $ supplyReq req $ unVerbs vmap


-- | Given a possible @Accept@ header and file extension key, lookup the contents
-- of a map.
lookupFileExt :: Maybe AcceptHeader -> FileExt -> FileExts a -> Maybe a
lookupFileExt mAccept k (FileExts xs) =
  let attempts = maybe [Html,Text,Json,JavaScript,Css]
                   (possibleFileExts k) mAccept
  in getFirst $ foldMap (First . flip Map.lookup xs) attempts


-- | Takes a subject file extension and an @Accept@ header, and returns the other
-- types of file types handleable, in order of prescedence.
possibleFileExts :: FileExt -> AcceptHeader -> [FileExt]
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

getVerb :: Request -> Verb
getVerb req = fromMaybe GET $ httpMethodToMSym $ requestMethod req


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
