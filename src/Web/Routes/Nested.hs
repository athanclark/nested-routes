{-# LANGUAGE
    GADTs
  , PolyKinds
  , TypeFamilies
  , DeriveFunctor
  , TypeOperators
  , TupleSections
  , DoAndIfThenElse
  , ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}

-- |
-- Module      : Web.Routes.Nested
-- Copyright   : (c) 2015 Athan Clark
--
-- License     : BSD-style
-- Maintainer  : athan.clark@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module exports most of what you'll need for sophisticated routing -
-- all the tools from <https://hackage.haskell.org/package/wai-middleware-verbs wai-middleware-verbs>
-- (routing for the incoming HTTP method) and
-- <https://hackage.haskell.org/package/wai-middleware-content-type wai-middleware-content-type>
-- (routing for the incoming Accept header, and implied file extension),
-- <https://hackage.haskell.org/package/wai WAI> itself, and
-- <https://hackage.haskell.org/package/wai-transformers wai-transformers> - some simple
-- type aliases wrapped around WAI's @Application@ and @Middleware@ types, allowing us
-- to embed monad transformer stacks for our applications.
--
-- The routing system lets you embed these complicated HTTP verb / content-type
-- sensative responses just as easily as a WAI @Middleware@. There is enough
-- tooling provided to use one paradigm or the other. Note - nested-routes
-- does not affect the @pathInfo@ of the incoming @Request@ in any way, but merely
-- matches on it and passes control to the designated response.
--
-- To match a route, you have a few options - you can match against a string literal,
-- a regular expression (via <https://hackage.haskell.org/package/regex-compat regex-compat>),
-- or an <https://hackage.haskell.org/package/attoparsec attoparsec> parser. This list
-- will most likely grow in the future, depending on demand.
--
-- There is also support for embedding security layers in your routes, in the same
-- nested manner. By "tagging" a set of routes with an authorization role (with @auth@),
-- you populate a list of roles breached during any request. In the authentication
-- parameter in @routeAuth@ and @routeActionAuth@, the function
-- keeps the session integrity in-place, while @auth@ lets you create your authorization
-- boundaries. Both are symbiotic and neccessary for establishing security, and both allow
-- you to tap into the monad transformer stack to do logging, STM, database queries,
-- etc.
--
-- To use your set of routes in a WAI application, you need to "extract" the
-- functionality from your route set - using the @route@, @routeAuth@, @routeAction@,
-- and @routeActionAuth@
-- functions, you can create monolithic apps very easily.
-- But, if you would like to extract the security middleware to place before
-- say, a /static/ middleware you already have in place, use the @extractAuth@
-- functions, and others for their respective purposes. This way, you can decompose
-- the routing system into each subject matter, and re-compose (@.@) them in whichever
-- order you like for your application.



module Web.Routes.Nested
  ( module X
  -- * Types
  , Tries
  , HandlerT (..)
  , execHandlerT
  , ActionT
  , RoutableT
  , RoutableActionT
  , AuthScope (..)
  , ExtrudeSoundly
  -- * Combinators
  , handle
  , handleAction
  , here
  , hereAction
  , handleAny
  , handleAnyAction
  , parent
  , auth
  , notFound
  , notFoundAction
  , action
  -- * Routing
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
import           Data.Monoid
import           Data.Foldable
import           Data.Functor.Syntax
import           Data.Function.Poly

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Control.Monad.State                as S


type Tries x s e = ( RootedPredTrie T.Text x
                   , RootedPredTrie T.Text x
                   , RootedPredTrie T.Text s
                   , RootedPredTrie T.Text e
                   )

newtype HandlerT x sec err aux m a = HandlerT
  { runHandlerT :: S.StateT (Tries x sec err) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadTrans
           , S.MonadState (Tries x sec err)
           )

execHandlerT :: Monad m => HandlerT x sec err aux m a -> m (Tries x sec err)
execHandlerT hs = S.execStateT (runHandlerT hs) mempty

type ActionT e u m a = VerbListenerT (FileExtListenerT (MiddlewareT m) m a) e u m a

-- | Turn an @ActionT@ into a @MiddlewareT@ - could be used to make middleware-based
-- route sets cooperate with the content-type and verb combinators.
action :: MonadIO m => ActionT e u m () -> MiddlewareT m
action xs = verbsToMiddleware $ mapVerbs fileExtsToMiddleware xs


type RoutableT s e u ue m a =
  HandlerT (MiddlewareT m) (s, AuthScope) (e -> MiddlewareT m) (e,u,ue) m a

type RoutableActionT s e u ue m a =
  HandlerT (ActionT ue u m ()) (s, AuthScope) (e -> ActionT ue u m ()) (e,u,ue) m a

type ExtrudeSoundly cleanxs xs c r =
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
                , HasResult childContent (ActionT ue u m ())
                , HasResult err          (e -> ActionT ue u m ())
                , Singleton (UrlChunks xs)
                    childContent
                    (RootedPredTrie T.Text resultContent)
                , cleanxs ~ CatMaybes xs
                , ArityTypeListIso childContent cleanxs resultContent
                ) => UrlChunks xs
                  -> childContent
                  -> HandlerT resultContent sec err (e,u,ue) m ()
handleAction ts vl = tell' (singleton ts vl, mempty, mempty, mempty)


-- | Embed a @MiddlewareT@ into a set of routes.
handle :: ( Monad m
          , Functor m
          , HasResult childContent (MiddlewareT m)
          , HasResult err     (e -> MiddlewareT m)
          , Singleton (UrlChunks xs)
              childContent
              (RootedPredTrie T.Text resultContent)
          , cleanxs ~ CatMaybes xs
          , ArityTypeListIso childContent cleanxs resultContent
          ) => UrlChunks xs
            -> childContent
            -> HandlerT resultContent sec err (e,u,ue) m ()
handle ts vl = tell' (singleton ts vl, mempty, mempty, mempty)


hereAction :: ( Monad m
              , Functor m
              , HasResult content (ActionT ue u m ())
              , HasResult err     (e -> ActionT ue u m ())
              ) => content
                -> HandlerT content sec err (e,u,ue) m ()
hereAction = handleAction o

-- | Create a handle for the present route - an alias for @\h -> handle o (Just h)@.
here :: ( Monad m
        , Functor m
        , HasResult content (MiddlewareT m)
        , HasResult err     (e -> MiddlewareT m)
        ) => content
          -> HandlerT content sec err (e,u,ue) m ()
here = handle o


handleAnyAction :: ( Monad m
                   , Functor m
                   , HasResult content (ActionT ue u m ())
                   , HasResult err     (e -> ActionT ue u m ())
                   ) => content
                     -> HandlerT content sec err (e,u,ue) m ()
handleAnyAction vl = tell' (mempty, singleton o vl, mempty, mempty)

-- | Match against any route, as a last resort against all failing @handle@s.
handleAny :: ( Monad m
             , Functor m
             , HasResult content (MiddlewareT m)
             , HasResult err     (e -> MiddlewareT m)
             ) => content
               -> HandlerT content sec err (e,u,ue) m ()
handleAny vl = tell' (mempty, singleton o vl, mempty, mempty)


-- | Prepend a path to an existing set of routes.
parent :: ( Monad m
          , Functor m
          , cleanxs ~ CatMaybes xs
          , ExtrudeSoundly cleanxs xs childContent resultContent
          , ExtrudeSoundly cleanxs xs childSec     resultSec
          , ExtrudeSoundly cleanxs xs childErr     resultErr
          ) => UrlChunks xs
            -> HandlerT childContent  childSec  childErr  aux m ()
            -> HandlerT resultContent resultSec resultErr aux m ()
parent ts cs = do
  (trieContent,trieNotFound,trieSec,trieErr) <- lift $ execHandlerT cs
  tell' ( extrude ts trieContent
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
  tell' ( mempty
        , mempty
        , RootedPredTrie (Just (token,scope)) mempty
        , RootedPredTrie (Just handleFail) mempty
        )


-- | Embed an @ActionT@ as a not-found handler into a set of routes, without first converting
-- it to a @MiddlewareT@.
notFoundAction :: ( Monad m
                  , Functor m
                  , HasResult content (ActionT ue u m ())
                  , HasResult err     (e -> ActionT ue u m ())
                  ) => content
                    -> HandlerT content sec err (e,u,ue) m ()
notFoundAction = handleAnyAction

-- | Embed a @MiddlewareT@ as a not-found handler into a set of routes.
notFound :: ( Monad m
            , Functor m
            , HasResult content (MiddlewareT m)
            , HasResult err     (e -> MiddlewareT m)
            ) => content
              -> HandlerT content sec err (e,u,ue) m ()
notFound = handleAny



-- * Routing ---------------------------------------

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
             ) => (Request -> [sec] -> m (Response -> Response, Maybe e)) -- ^ authorize
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
                   ) => (Request -> [sec] -> m (Response -> Response, Maybe e)) -- ^ authorize
                     -> RoutableActionT sec e u ue m () -- ^ Assembled @handle@ calls
                     -> MiddlewareT m
routeActionAuth authorize = routeAuth authorize . actionToMiddleware


-- | Turns a @HandlerT@ containing @ActionT@s into a @HandlerT@ containing @MiddlewareT@s.
actionToMiddleware :: MonadIO m =>
                      RoutableActionT sec e u ue m ()
                   -> RoutableT sec e u ue m ()
actionToMiddleware hs = do
  (rtrie,nftrie,strie,errtrie) <- lift $ execHandlerT hs
  tell' ( action <$> rtrie
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
               ) => (Request -> [sec] -> m (Response -> Response, Maybe e)) -- authorization method
                 -> HandlerT x (sec, AuthScope) (e -> MiddlewareT m) aux m a
                 -> MiddlewareT m
extractAuth authorize hs app req respond = do
  (_,_,_,trie) <- execHandlerT hs
  ss <- extractAuthSym hs req
  (f,me) <- authorize req ss
  fromMaybe (app req (respond . f)) $ do
    e <- me
    (_,mid,_) <- PT.matchRPT (pathInfo req) trie
    return $ mid e app req (respond . f)

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

tell' :: (Monoid w, S.MonadState w m) => w -> m ()
tell' x = do
  xs <- S.get
  S.put $ xs <> x
