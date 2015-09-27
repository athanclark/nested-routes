{-# LANGUAGE
    GADTs
  , PolyKinds
  , TypeFamilies
  , DeriveFunctor
  , TypeOperators
  , TupleSections
  , ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UndecidableInstances
  , GeneralizedNewtypeDeriving
  #-}

module Web.Routes.Nested
  ( -- * Types
    module X
  , Tries
  , HandlerT (..)
  , execHandlerT
  , ActionT
  , RoutableT
  , RoutableActionT
  , AuthScope (..)
  , ExtrudeSound
  -- * Combinators
  , handle
  , handleAction
  , parent
  , auth
  , notFound
  , notFoundAction
  , action
  -- * Entry Point
  , route
  , routeAuth
  , routeAction
  , routeActionAuth
  -- * Extraction
  , extractContent
  , extractNotFound
  , extractAuthSym
  , extractAuth
  , extractNearestVia
  , actionToMiddleware
  ) where

import           Web.Routes.Nested.Types            as X
import           Network.Wai.Trans                  as X
import           Network.Wai.Middleware.Verbs       as X
import           Network.Wai.Middleware.ContentType as X

import           Data.Trie.Pred                     (RootedPredTrie (..), PredTrie (..))
import qualified Data.Trie.Pred                     as PT -- only using lookups
import           Data.Trie.Pred.Step                (PredStep (..), PredSteps (..))
import qualified Data.Trie.Class                    as TC
import           Data.Trie.Map                      (MapStep (..))
import qualified Data.Map                           as Map
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import           Data.Maybe                         (fromMaybe)
import           Data.Foldable
import           Data.Functor.Syntax
import           Data.Function.Poly

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Writer
import qualified Control.Monad.State                as S


type Tries x s e = ( RootedPredTrie T.Text x
                   , RootedPredTrie T.Text x
                   , RootedPredTrie T.Text s
                   , RootedPredTrie T.Text e
                   )

newtype HandlerT x sec err aux m a = HandlerT
  { runHandlerT :: WriterT (Tries x sec err) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadTrans
           , MonadWriter (Tries x sec err)
           )

execHandlerT :: Monad m => HandlerT x sec err aux m a -> m (Tries x sec err)
execHandlerT = execWriterT . runHandlerT

type ActionT e u m a = VerbListenerT (FileExtListenerT (MiddlewareT m) m a) e u m a

action :: MonadIO m => ActionT e u m () -> MiddlewareT m
action xs = verbsToMiddleware $ mapVerbs fileExtsToMiddleware xs


type RoutableT s e u ue m a =
  HandlerT (MiddlewareT m) (s, AuthScope) (e -> MiddlewareT m) (e,u,ue) m a

type RoutableActionT s e u ue m a =
  HandlerT (ActionT ue u m ()) (s, AuthScope) (e -> ActionT ue u m ()) (e,u,ue) m a

type ExtrudeSound cleanxs xs c r =
  ( cleanxs ~ CatMaybes xs
  , ArityTypeListIso c cleanxs r
  , Extrude (UrlChunks xs)
      (RootedPredTrie T.Text c)
      (RootedPredTrie T.Text r)
  )


-- | Embed an @ActionT@ into a set of routes directly, without first converting
-- it to a @MiddlewareT@.
handleAction :: ( Monad m
                , Functor m
                , cleanxs ~ CatMaybes xs
                , HasResult childContent (ActionT ue u m ())
                , HasResult childErr     (e -> ActionT ue u m ())
                , ExpectArity cleanxs childContent
                , ExpectArity cleanxs childSec
                , ExpectArity cleanxs childErr
                , Singleton (UrlChunks xs)
                    childContent
                    (RootedPredTrie T.Text resultContent)
                , ExtrudeSound cleanxs xs childContent resultContent
                , ExtrudeSound cleanxs xs childSec     resultSec
                , ExtrudeSound cleanxs xs childErr     resultErr
                ) => UrlChunks xs
                  -> Maybe childContent
                  -> Maybe (HandlerT childContent  childSec  childErr  (e,u,ue) m ())
                  ->        HandlerT resultContent resultSec resultErr (e,u,ue) m ()
handleAction ts (Just vl) Nothing = tell (singleton ts vl, mempty, mempty, mempty)
handleAction ts mvl (Just cs) = do
  (RootedPredTrie _ trieContent,trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts $ RootedPredTrie mvl trieContent
       , extrude ts trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )
handleAction _ Nothing Nothing = return ()

-- | Embed a @MiddlewareT@ into a set of routes.
handle :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , HasResult childContent (MiddlewareT m)
          , HasResult childErr     (e -> MiddlewareT m)
          , ExpectArity cleanxs childContent
          , ExpectArity cleanxs childSec
          , ExpectArity cleanxs childErr
          , Singleton (UrlChunks xs)
              childContent
              (RootedPredTrie T.Text resultContent)
          , ExtrudeSound cleanxs xs childContent resultContent
          , ExtrudeSound cleanxs xs childSec     resultSec
          , ExtrudeSound cleanxs xs childErr     resultErr
          ) => UrlChunks xs
            -> Maybe childContent
            -> Maybe (HandlerT childContent  childSec  childErr  (e,u,ue) m ())
            ->        HandlerT resultContent resultSec resultErr (e,u,ue) m ()
handle ts (Just vl) Nothing = tell (singleton ts vl, mempty, mempty, mempty)
handle ts mvl (Just cs) = do
  (RootedPredTrie _ trieContent,trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts $ RootedPredTrie mvl trieContent
       , extrude ts trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )
handle _ Nothing Nothing = return ()

-- | Prepend a path to an existing set of routes.
parent :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , Singleton (UrlChunks xs)
              childContent
              (RootedPredTrie T.Text resultContent)
          , ExtrudeSound cleanxs xs childContent resultContent
          , ExtrudeSound cleanxs xs childSec     resultSec
          , ExtrudeSound cleanxs xs childErr     resultErr
          ) => UrlChunks xs
            -> HandlerT childContent  childSec  childErr  aux m ()
            -> HandlerT resultContent resultSec resultErr aux m ()
parent ts cs = do
  (trieContent,trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts trieContent
       , extrude ts trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )


-- | Designate the scope of security to the set of routes - either only the adjacent
-- routes, or the adjacent /and/ the parent container node (root node if not
-- declared).
data AuthScope = ProtectParent | ProtectChildren
  deriving (Show, Eq)

-- | Sets the security role and error handler for a set of routes, optionally
-- including its parent route.
auth :: ( Monad m
        , Functor m
        ) => sec
          -> err
          -> AuthScope
          -> HandlerT content (sec, AuthScope) err aux m ()
auth token handleFail scope =
  tell ( mempty
       , mempty
       , RootedPredTrie (Just (token,scope)) mempty
       , RootedPredTrie (Just handleFail) mempty
       )


-- | Embed an @ActionT@ as a not-found handler into a set of routes, without first converting
-- it to a @MiddlewareT@.
notFoundAction :: ( Monad m
                  , Functor m
                  , cleanxs ~ CatMaybes xs
                  , HasResult childContent (ActionT ue u m ())
                  , HasResult childErr     (e -> ActionT ue u m ())
                  , ExpectArity cleanxs childContent
                  , ExpectArity cleanxs childSec
                  , ExpectArity cleanxs childErr
                  , Singleton (UrlChunks xs)
                      childContent
                      (RootedPredTrie T.Text resultContent)
                  , ExtrudeSound cleanxs xs childContent resultContent
                  , ExtrudeSound cleanxs xs childSec     resultSec
                  , ExtrudeSound cleanxs xs childErr     resultErr
                  ) => UrlChunks xs
                    -> Maybe childContent
                    -> Maybe (HandlerT childContent  childSec  childErr  (e,u,ue) m ())
                    ->        HandlerT resultContent resultSec resultErr (e,u,ue) m ()
notFoundAction ts (Just vl) Nothing = tell (mempty, singleton ts vl, mempty, mempty)
notFoundAction ts mvl (Just cs) = do
  (trieContent,RootedPredTrie _ trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts trieContent
       , extrude ts $ RootedPredTrie mvl trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )
notFoundAction _ Nothing Nothing = return ()

-- | Embed a @MiddlewareT@ as a not-found handler into a set of routes.
notFound :: ( Monad m
            , Functor m
            , cleanxs ~ CatMaybes xs
            , HasResult childContent (MiddlewareT m)
            , HasResult childErr     (e -> MiddlewareT m)
            , ExpectArity cleanxs childContent
            , ExpectArity cleanxs childSec
            , ExpectArity cleanxs childErr
            , Singleton (UrlChunks xs)
                childContent
                (RootedPredTrie T.Text resultContent)
            , ExtrudeSound cleanxs xs childContent resultContent
            , ExtrudeSound cleanxs xs childSec     resultSec
            , ExtrudeSound cleanxs xs childErr     resultErr
            ) => UrlChunks xs
              -> Maybe childContent
              -> Maybe (HandlerT childContent  childSec  childErr  (e,u,ue) m ())
              ->        HandlerT resultContent resultSec resultErr (e,u,ue) m ()
notFound ts (Just vl) Nothing = tell (mempty, singleton ts vl, mempty, mempty)
notFound ts mvl (Just cs) = do
  (trieContent,RootedPredTrie _ trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell ( extrude ts trieContent
       , extrude ts $ RootedPredTrie mvl trieNotFound
       , extrude ts trieSec
       , extrude ts trieErr
       )
notFound _ Nothing Nothing = return ()


-- * Entry Point ---------------------------------------

-- | Turns a @HandlerT@ containing @MiddlewareT@s into a @MiddlewareT@.
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) => HandlerT (MiddlewareT m) sec err aux m () -- ^ Assembled @handle@ calls
           -> MiddlewareT m
route hs = extractContent hs . extractNotFound hs


-- | Given a security verification function that returns a method to updating the session,
-- turn a set of routes containing @MiddlewareT@s into a @MiddlewareT@, where a session
-- is secured before responding.
routeAuth :: ( Functor m
             , Monad m
             , MonadIO m
             ) => (Request -> [sec] -> ExceptT e m (Response -> Response)) -- ^ authorize
               -> RoutableT sec e u ue m () -- ^ Assembled @handle@ calls
               -> MiddlewareT m
routeAuth authorize hs = extractAuth authorize hs . route hs

-- | Exactly like @route@, except specialized to route sets that contain @ActionT@s -
-- essentially @fmap@ing @action@ to each element.
routeAction :: ( Functor m
               , Monad m
               , MonadIO m
               ) => RoutableActionT sec e u ue m ()
                 -> MiddlewareT m
routeAction = route . actionToMiddleware

-- | Exactly like @routeAuth@, but specialized for @ActionT@.
routeActionAuth :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => (Request -> [sec] -> ExceptT e m (Response -> Response)) -- ^ authorize
                     -> RoutableActionT sec e u ue m () -- ^ Assembled @handle@ calls
                     -> MiddlewareT m
routeActionAuth authorize = routeAuth authorize . actionToMiddleware


-- | Turns a @HandlerT@ containing @ActionT@s into a @HandlerT@ containing @MiddlewareT@s.
actionToMiddleware :: MonadIO m =>
                      RoutableActionT sec e u ue m ()
                   -> RoutableT sec e u ue m ()
actionToMiddleware hs = do
  (rtrie,nftrie,strie,errtrie) <- lift $ execHandlerT hs
  tell ( action <$> rtrie
       , action <$> nftrie
       , strie
       , (action .) <$> errtrie
       )


-- * Extraction -------------------------------

-- | Extracts only the normal @handle@ (content) routes into
-- a @MiddlewareT@, disregarding security and not-found responses.
extractContent :: ( Functor m
                  , Monad m
                  , MonadIO m
                  ) => HandlerT (MiddlewareT m) sec err aux m a
                    -> MiddlewareT m
extractContent hs app req respond = do
  (trie,_,_,_) <- execHandlerT hs
  case lookupWithLRPT trimFileExt (pathInfo req) trie of
    Nothing -> fromMaybe (app req respond) $ do
      guard $ not . null $ pathInfo req
      guard $ trimFileExt (last $ pathInfo req) == "index"
      mid <- TC.lookup (init $ pathInfo req) trie
      Just $    mid app req respond
    Just mid -> mid app req respond


-- | Find the security tokens / authorization roles affiliated with
-- a request for a set of routes.
extractAuthSym :: ( Functor m
                  , Monad m
                  ) => HandlerT x (sec, AuthScope) err aux m a
                    -> Request
                    -> m [sec]
extractAuthSym hs req = do
  (_,_,trie,_) <- execHandlerT hs
  return $ foldl go [] (PT.matchesRPT (pathInfo req) trie)
  where
    go ys (_,(_,ProtectChildren),[]) = ys
    go ys (_,(x,_              ),_ ) = ys ++ [x]

-- | Extracts only the security handling logic into a @MiddlewareT@.
extractAuth :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => (Request -> [sec] -> ExceptT e m (Response -> Response)) -- authorization method
                     -> HandlerT x (sec, AuthScope) (e -> MiddlewareT m) aux m a
                     -> MiddlewareT m
extractAuth authorize hs app req respond = do
  (_,_,_,trie) <- execHandlerT hs
  ss <- extractAuthSym hs req
  ef <- runExceptT $ authorize req ss
  either (\e -> maybe (app req respond)
                      (\mid -> mid app req respond)
                    $ (getResultsFromMatch <$> PT.matchRPT (pathInfo req) trie) <$~> e)
         (\f -> app req (respond . f))
         ef

-- | Extracts only the @notFound@ responses into a @MiddlewareT@.
extractNotFound :: ( Functor m
                   , Monad m
                   , MonadIO m
                   ) => HandlerT (MiddlewareT m) sec err aux m a
                     -> MiddlewareT m
extractNotFound = extractNearestVia (execHandlerT >=> \(_,t,_,_) -> return t)


-- | Given a way to draw out a special-purpose trie from our route set, route
-- to the responses based on a /furthest-reached/ method.
extractNearestVia :: ( Functor m
                     , Monad m
                     , MonadIO m
                     ) => (HandlerT (MiddlewareT m) sec err aux m a -> m (RootedPredTrie T.Text (MiddlewareT m)))
                       -> HandlerT (MiddlewareT m) sec err aux m a
                       -> MiddlewareT m
extractNearestVia extr hs app req respond = do
  trie <- extr hs
  maybe (app req respond)
        (\mid -> mid app req respond)
      $ getResultsFromMatch <$> PT.matchRPT (pathInfo req) trie


getResultsFromMatch :: ([s],a,[s]) -> a
getResultsFromMatch (_,x,_) = x



-- * Pred-Trie related -----------------

-- | Removes @.txt@ from @foo.txt@
trimFileExt :: T.Text -> T.Text
trimFileExt s =
  let lastExt = getLastExt (T.unpack s)
  in if lastExt `elem` possibleExts
     then T.pack lastExt
     else s
  where
    possibleExts = [ ".html",".htm",".txt",".json",".lucid"
                   , ".julius",".css",".cassius",".lucius"
                   ]
    getLastExt ts = S.evalState (foldrM go [] ts) False
      where
        go c soFar = do
          sawPeriod <- S.get
          if sawPeriod
          then return soFar
          else if c == '.'
               then do S.put True
                       return ('.' : soFar)
               else    return (c : soFar)


-- | A quirky function for processing the last element of a lookup path, only
-- on /literal/ matches.
lookupWithLPT :: Ord s => (s -> s) -> NonEmpty s -> PredTrie s a -> Maybe a
lookupWithLPT f (t:|ts) (PredTrie (MapStep ls) (PredSteps ps))
  | null ts   = getFirst $ First (goLit (f t) ls) <> foldMap (First . goPred) ps
  | otherwise = getFirst $ First (goLit    t  ls) <> foldMap (First . goPred) ps
  where
    goLit t' xs = do
      (mx,mxs) <- Map.lookup t' xs
      if null ts
      then mx
      else lookupWithLPT f (NE.fromList ts) =<< mxs

    goPred (PredStep _ predicate mx xs) = do
      d <- predicate t
      if null ts
      then mx <$~> d
      else lookupWithLPT f (NE.fromList ts) xs <$~> d

lookupWithLRPT :: Ord s => (s -> s) -> [s] -> RootedPredTrie s a -> Maybe a
lookupWithLRPT _ [] (RootedPredTrie mx _) = mx
lookupWithLRPT f ts (RootedPredTrie _ xs) = lookupWithLPT f (NE.fromList ts) xs
