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

{- |
Module      : Web.Routes.Nested
Copyright   : (c) 2015 Athan Clark

License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

This module exports most of what you'll need for sophisticated routing -
all the tools from <https://hackage.haskell.org/package/wai-middleware-verbs wai-middleware-verbs>
(routing for the incoming HTTP method) and
<https://hackage.haskell.org/package/wai-middleware-content-type wai-middleware-content-type>
(routing for the incoming Accept header, and implied file extension),
<https://hackage.haskell.org/package/wai WAI> itself, and
<https://hackage.haskell.org/package/wai-transformers wai-transformers> - some simple
type aliases wrapped around WAI's @Application@ and @Middleware@ types, allowing us
to embed monad transformer stacks for our applications.

The routing system lets you embed these complicated HTTP verb / content-type
sensative responses just as easily as a WAI @Middleware@. There is enough
tooling provided to use one paradigm or the other. Note - nested-routes
does not affect the @pathInfo@ of the incoming @Request@ in any way, but merely
matches on it and passes control to the designated response.

To match a route, you have a few options - you can match against a string literal,
a regular expression (via <https://hackage.haskell.org/package/regex-compat regex-compat>),
or an <https://hackage.haskell.org/package/attoparsec attoparsec> parser. This list
will most likely grow in the future, depending on demand.

There is also support for embedding security layers in your routes, in the same
nested manner. By "tagging" a set of routes with an authorization role (with @auth@),
you populate a list of roles breached during any request. In the authentication
parameter in @routeAuth@ and @routeActionAuth@, the function
keeps the session integrity in-place, while @auth@ lets you create your authorization
boundaries. Both are symbiotic and neccessary for establishing security, and both allow
you to tap into the monad transformer stack to do logging, STM, database queries,
etc.

To use your set of routes in a WAI application, you need to "extract" the
functionality from your route set - using the @route@, @routeAuth@, @routeAction@,
and @routeActionAuth@
functions, you can create monolithic apps very easily.
But, if you would like to extract the security middleware to place before
say, a /static/ middleware you already have in place, use the @extractAuth@
functions, and others for their respective purposes. This way, you can decompose
the routing system into each subject matter, and re-compose (@.@) them in whichever
order you like for your application.
-}


module Web.Routes.Nested
  ( module X
  -- * Types
  , Tries
  , HandlerT (..)
  , execHandlerT
  , ActionT
  , RoutableT
  , AuthScope (..)
  , ExtrudeSoundly
  -- * Combinators
  , match
  , matchHere
  , matchAny
  , matchGroup
  , auth
  , action
  -- * Routing
  , route
  , routeAuth
  -- * Extraction
  , extractMatch
  , extractMatchAny
  , extractAuthSym
  , extractAuth
  , extractNearestVia
  ) where

import           Web.Routes.Nested.Types            as X
import           Network.Wai.Trans                  as X
import           Network.Wai.Middleware.Verbs       as X
import           Network.Wai.Middleware.ContentType as X

import           Data.Trie.Pred                     (RootedPredTrie (..), PredTrie (..))
import qualified Data.Trie.Pred                     as PT -- only using lookups
import           Data.Trie.Pred.Step                (PredStep (..), PredSteps (..))
import qualified Data.Trie.Class                    as TC
import           Data.Trie.HashMap                  (HashMapStep (..))
import qualified Data.HashMap.Lazy                  as HM
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import           Data.Hashable
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid
import           Data.Functor.Syntax
import           Data.Function.Poly

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Control.Monad.State                as S
import           Control.Monad.Catch


type Tries x s = ( RootedPredTrie T.Text x
                 , RootedPredTrie T.Text x
                 , RootedPredTrie T.Text s
                 )

newtype HandlerT x sec m a = HandlerT
  { runHandlerT :: S.StateT (Tries x sec) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadTrans
           , S.MonadState (Tries x sec)
           )

execHandlerT :: Monad m => HandlerT x sec m a -> m (Tries x sec)
execHandlerT hs = S.execStateT (runHandlerT hs) mempty

type ActionT m a = VerbListenerT (FileExtListenerT (MiddlewareT m) m a) m a

-- | Turn an @ActionT@ into a @MiddlewareT@ - could be used to make middleware-based
-- route sets cooperate with the content-type and verb combinators.
action :: MonadIO m => ActionT m () -> MiddlewareT m
action xs = verbsToMiddleware $ mapVerbs fileExtsToMiddleware xs


type RoutableT s m a = HandlerT (MiddlewareT m) (s, AuthScope) m a

type ExtrudeSoundly cleanxs xs c r =
  ( cleanxs ~ CatMaybes xs
  , ArityTypeListIso c cleanxs r
  , Extrude (UrlChunks xs)
      (RootedPredTrie T.Text c)
      (RootedPredTrie T.Text r)
  )

-- | Embed a 'Network.Wai.Trans.MiddlewareT' into a set of routes via a matching string. You should
--   expect the match to create /arity/ in your handler - the @childContent@ variable.
--   The arity of @childContent@ may grow or shrink, depending on the heterogeneous
--   list created from the list of parsers, regular expressions, or arbitrary predicates
--   /in the order written/ - so something like:
--
--   > match (p_ ("double-parser", Data.Attoparsec.Text.double </> o_))
--   >   handle
--
--   ...then @handle@ /must/ have an arity __ending__ in @Double ->@ - if this
--   route was at the top level, then the total arity __must__ be @Double -> MiddlewareT m@.
--   (notice 'Web.Routes.Nested.Types.HasResult' -
--
--   > HasResult childContent (MiddlewareT m)
--
--   Basically, if the routes you are building in the current scope get grouped
--   by a predicate (which creates another data type to handle) with 'group',
--   then we would need another variable of arity /before/ the @Double@.
match :: ( Monad m
         , HasResult childContent (MiddlewareT m)
         , Singleton (UrlChunks xs)
             childContent
             (RootedPredTrie T.Text resultContent)
         , cleanxs ~ CatMaybes xs
         , ArityTypeListIso childContent cleanxs resultContent
         ) => UrlChunks xs
           -> childContent
           -> HandlerT resultContent sec m ()
match ts vl = tell' (singleton ts vl, mempty, mempty)


-- | Create a handle for the /current/ route - an alias for @\h -> match o_ handle@.
matchHere :: ( Monad m
             , HasResult content (MiddlewareT m)
             ) => content
               -> HandlerT content sec m ()
matchHere = match origin_


-- | Match against any route, as a last resort against all failing matches -
--   most people use this for a catch-all at some level in their routes, something
--   like a @not-found 404@ page is useful.
matchAny :: ( Monad m
            , HasResult content (MiddlewareT m)
            ) => content
              -> HandlerT content sec m ()
matchAny vl = tell' (mempty, singleton origin_ vl, mempty)


-- | Prepends a common route to an existing set of routes. You should note that
--   doing this with a parser or regular expression will necessitate the existing
--   arity in the handlers before the progam can compile.
matchGroup :: ( Monad m
              , cleanxs ~ CatMaybes xs
              , ExtrudeSoundly cleanxs xs childContent resultContent
              , ExtrudeSoundly cleanxs xs childSec     resultSec
              ) => UrlChunks xs
                -> HandlerT childContent  childSec  m ()
                -> HandlerT resultContent resultSec m ()
matchGroup ts cs = do
  (trieContent,trieNotFound,trieSec) <- lift $ execHandlerT cs
  tell' ( extrude ts trieContent
        , extrude ts trieNotFound
        , extrude ts trieSec
        )


-- | Designate the scope of security to the set of routes - either only the adjacent
-- routes, or the adjacent /and/ the parent container node (root node if not
-- declared).
data AuthScope = ProtectHere | DontProtectHere
  deriving (Show, Eq)

-- | Sets the security role and error handler for a set of routes, optionally
-- including its parent route.
auth :: ( Monad m
        ) => sec
          -> AuthScope
          -> HandlerT content (sec, AuthScope) m ()
auth token scope =
  tell' ( mempty
        , mempty
        , singleton origin_ (token,scope)
        )


-- * Routing ---------------------------------------

-- | Turns a @HandlerT@ containing @MiddlewareT@s into a @MiddlewareT@.
route :: ( MonadIO m
         ) => HandlerT (MiddlewareT m) sec m () -- ^ Assembled @handle@ calls
           -> MiddlewareT m
route hs = extractMatch hs . extractMatchAny hs


-- | Given a security verification function that returns a method to updating the session,
-- turn a set of routes containing @MiddlewareT@s into a @MiddlewareT@, where a session
-- is secured before responding.
routeAuth :: ( MonadIO m
             , MonadThrow m
             ) => (Request -> [sec] -> m (Response -> Response)) -- ^ authorize
               -> RoutableT sec m () -- ^ Assembled @handle@ calls
               -> MiddlewareT m
routeAuth authorize hs = extractAuth authorize hs . route hs



-- * Extraction -------------------------------

-- | Extracts only the normal 'match' and 'matchHere' (exactly matching content) routes into
-- a @MiddlewareT@, disregarding security and not-found responses.
extractMatch :: ( MonadIO m
                ) => HandlerT (MiddlewareT m) sec m a
                  -> MiddlewareT m
extractMatch hs app req respond = do
  (trie,_,_) <- execHandlerT hs
  case matchWithLRPT trimFileExt (pathInfo req) trie of
    Nothing -> fromMaybe (app req respond) $ do
      guard $ not . null $ pathInfo req
      guard $ trimFileExt (last $ pathInfo req) == "index"
      mid <- TC.lookup (init $ pathInfo req) trie
      Just $    mid app req respond
    Just (_,mid) -> mid app req respond


-- | Extracts only the @notFound@ responses into a @MiddlewareT@.
extractMatchAny :: ( MonadIO m
                   ) => HandlerT (MiddlewareT m) sec m a
                     -> MiddlewareT m
extractMatchAny = extractNearestVia (execHandlerT >=> \(_,t,_) -> return t)



-- | Find the security tokens / authorization roles affiliated with
-- a request for a set of routes.
extractAuthSym :: ( Monad m
                  ) => HandlerT x (sec, AuthScope) m a
                    -> Request
                    -> m [sec]
extractAuthSym hs req = do
  (_,_,trie) <- execHandlerT hs
  return $ foldl go [] (PT.matchesRPT (pathInfo req) trie)
  where
    go ys (_,(_,DontProtectHere),[]) = ys
    go ys (_,(x,_              ),_ ) = ys ++ [x]

-- | Extracts only the security handling logic into a @MiddlewareT@.
extractAuth :: ( MonadIO m
               , MonadThrow m
               ) => (Request -> [sec] -> m (Response -> Response)) -- authorization method
                 -> HandlerT x (sec, AuthScope) m a
                 -> MiddlewareT m
extractAuth authorize hs app req respond = do
  ss <- extractAuthSym hs req
  f <- authorize req ss
  app req (respond . f)


-- | Given a way to draw out a special-purpose trie from our route set, route
-- to the responses based on a /furthest-reached/ method.
extractNearestVia :: ( MonadIO m
                     ) => (HandlerT (MiddlewareT m) sec m a -> m (RootedPredTrie T.Text (MiddlewareT m)))
                       -> HandlerT (MiddlewareT m) sec m a
                       -> MiddlewareT m
extractNearestVia extr hs app req respond = do
  trie <- extr hs
  maybe (app req respond)
        (\(_,mid,_) -> mid app req respond)
      $ PT.matchRPT (pathInfo req) trie



-- * Pred-Trie related -----------------

-- | Removes @.txt@ from @foo.txt@
trimFileExt :: T.Text -> T.Text
trimFileExt s = fst (T.breakOn "." s)


-- | A quirky function for processing the last element of a lookup path, only
-- on /literal/ matches.
matchWithLPT :: ( Hashable s
                , Eq s
                ) => (s -> s) -> NonEmpty s -> PredTrie s a -> Maybe ([s], a)
matchWithLPT f (t:|ts) (PredTrie (HashMapStep ls) (PredSteps ps))
  | null ts   = getFirst $ First (goLit (f t) ls) <> foldMap (First . goPred) ps
  | otherwise = getFirst $ First (goLit    t  ls) <> foldMap (First . goPred) ps
  where
    goLit t' xs = do
      (mx,mxs) <- HM.lookup t' xs
      if null ts
      then ([t],) <$> mx
      else (\(ts',x) -> (t:ts',x)) <$> (matchWithLPT f (NE.fromList ts) =<< mxs)

-- TODO: Need `match` for Map? idk actually, `t` should be enough for this step.

    goPred (PredStep _ predicate mx xs) = do
      d <- predicate t
      if null ts
      then ([t],) <$> (mx <$~> d)
      else (\(ts',x) -> (t:ts',x d)) <$> (matchWithLPT f (NE.fromList ts) xs)

matchWithLRPT :: ( Hashable s
                 , Eq s
                 ) => (s -> s) -> [s] -> RootedPredTrie s a -> Maybe ([s], a)
matchWithLRPT _ [] (RootedPredTrie mx _) = ([],) <$> mx
matchWithLRPT f ts (RootedPredTrie _ xs) = matchWithLPT f (NE.fromList ts) xs


tell' :: (Monoid w, S.MonadState w m) => w -> m ()
tell' x = do
  xs <- S.get
  S.put $ xs <> x

