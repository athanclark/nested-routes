{-# LANGUAGE
    GADTs
  , PolyKinds
  , TypeFamilies
  , BangPatterns
  , TypeOperators
  , TupleSections
  , DoAndIfThenElse
  , ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , NamedFieldPuns
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

To match a route, you have a few options - you can match against a string literal,
a regular expression (via <https://hackage.haskell.org/package/regex-compat regex-compat>),
or an <https://hackage.haskell.org/package/attoparsec attoparsec> parser. This list
will most likely grow in the future, depending on demand.

There is also support for embedding security layers in your routes, in the same
nested manner. By "tagging" a set of routes with an authorization role (with @auth@),
you populate a list of roles breached during any request. The function argument to
'routeAuth' guards a Request to pass or fail at the high level, while 'auth' lets
you create your authorization boundaries on a case-by-case basis. Both allow
you to tap into the monad transformer stack for logging, STRefs, database queries,
etc.
-}


module Web.Routes.Nested
  ( -- * Router Construction
    match
  , matchHere
  , matchAny
  , matchGroup
  , auth
  , -- * Routing Middleware
    route
  , routeAuth
  , -- ** Precise Route Extraction
    extractMatch
  , extractMatchAny
  , extractAuthSym
  , extractAuth
  , extractNearestVia
  , -- * Metadata
    SecurityToken (..)
  , AuthScope (..)
  , Match
  , MatchGroup
  , -- * Re-Exports
    module Web.Routes.Nested.Match
  , module Web.Routes.Nested.Types
  , module Network.Wai.Middleware.Verbs
  , module Network.Wai.Middleware.ContentType
  ) where

import           Web.Routes.Nested.Match            (UrlChunks, origin_)
import           Web.Routes.Nested.Match
import           Web.Routes.Nested.Types            (RouterT, execRouterT, Tries (..), ExtrudeSoundly)
import           Web.Routes.Nested.Types
import           Network.Wai.Trans                  (MiddlewareT, Request, pathInfo)
import           Network.Wai.Middleware.Verbs
import           Network.Wai.Middleware.ContentType hiding (responseStatus, responseHeaders, responseData)

import           Data.Trie.Pred.Base                (RootedPredTrie (..), PredTrie (..))
import           Data.Trie.Pred.Base.Step           (PredStep (..), PredSteps (..))
import qualified Data.Trie.Pred.Interface           as Interface
import           Data.Trie.Pred.Interface.Types     (Singleton (..), Extrude (..), CatMaybes)
import           Data.Trie.HashMap                  (HashMapStep (..), HashMapChildren (..))
import           Data.List.NonEmpty                 (NonEmpty (..), fromList)
import qualified Data.Text                          as T
import           Data.Hashable                      (Hashable)
import qualified Data.HashMap.Strict                as HM
import           Data.Monoid                        ((<>), First (..))
import           Data.Function.Poly                 (ArityTypeListIso)
import           Data.Bifunctor                     (bimap)

import qualified Control.Monad.State                as S
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.Trans                (MonadTrans (..))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Control.Arrow                      (first)
import           Control.Monad.ST                   (stToIO)


-- | The constraints necessary for 'match'.
type Match xs' xs childContent resultContent =
  ( xs' ~ CatMaybes xs
  , Singleton (UrlChunks xs) childContent (RootedPredTrie T.Text resultContent)
  , ArityTypeListIso childContent xs' resultContent
  )


-- | The constraints necessary for 'matchGroup'.
type MatchGroup xs' xs childContent resultContent childSec resultSec =
  ( ExtrudeSoundly xs' xs childContent resultContent
  , ExtrudeSoundly xs' xs childSec     resultSec
  )


-- | Embed a 'Network.Wai.Trans.MiddlewareT' into a set of routes via a matching string. You should
--   expect the match to create /arity/ in your handler - the @childContent@ variable.
--   The arity of @childContent@ may grow or shrink, depending on the heterogeneous
--   list created from the list of parsers, regular expressions, or arbitrary predicates
--   /in the order written/ - so something like:
--
--   > match (p_ "double-parser" double </> o_)
--   >   handler
--
--   ...then @handler@ /must/ have arity @Double ->@. If this
--   route was at the top level, then the total arity __must__ be @Double -> MiddlewareT m@.
--
--   Generally, if the routes you are building get grouped
--   by a predicate with 'matchGroup',
--   then we would need another level of arity /before/ the @Double@.
match :: ( Monad m
         , Match xs' xs childContent resultContent
         ) => UrlChunks xs -- ^ Predicative path to match against
           -> childContent -- ^ The response to send
           -> RouterT resultContent sec m ()
match !ts !vl =
  tell' $ Tries (singleton ts vl)
                mempty
                mempty


{-# INLINEABLE match #-}

-- | Create a handle for the /current/ route - an alias for @\h -> match o_ h@.
matchHere :: ( Monad m
             ) => childContent -- ^ The response to send
               -> RouterT childContent sec m ()
matchHere = match origin_

{-# INLINEABLE matchHere #-}


-- | Match against any route, as a last resort against all failing matches -
--   use this for a catch-all at some level in their routes, something
--   like a @not-found 404@ page is useful.
matchAny :: ( Monad m
            ) => childContent -- ^ The response to send
              -> RouterT childContent sec m ()
matchAny !vl =
  tell' $ Tries mempty
                (singleton origin_ vl)
                mempty


{-# INLINEABLE matchAny #-}


-- | Prepends a common route to an existing set of routes. You should note that
--   doing this with a parser or regular expression will necessitate the existing
--   arity in the handlers before the progam can compile.
matchGroup :: ( Monad m
              , MatchGroup xs' xs childContent resultContent childSec resultSec
              ) => UrlChunks xs -- ^ Predicative path to match against
                -> RouterT childContent  childSec  m () -- ^ Child routes to nest
                -> RouterT resultContent resultSec m ()
matchGroup !ts cs = do
  (Tries trieContent' trieNotFound trieSec) <- lift $ execRouterT cs
  tell' $ Tries (extrude ts trieContent')
                (extrude ts trieNotFound)
                (extrude ts trieSec)


{-# INLINEABLE matchGroup #-}

-- | Use a custom security token type and an 'AuthScope' to define
--   /where/ and /what kind/ of security should take place.
data SecurityToken s = SecurityToken
  { securityToken :: !s
  , securityScope :: !AuthScope
  } deriving (Show)

-- | Designate the scope of security to the set of routes - either only the adjacent
-- routes, or the adjacent /and/ the parent container node (root node if not
-- declared).
data AuthScope
  = ProtectHere
  | DontProtectHere
  deriving (Show, Eq)

-- | Sets the security role and error handler for a set of routes, optionally
-- including its parent route.
auth :: ( Monad m
        ) => sec -- ^ Your security token
          -> AuthScope
          -> RouterT content (SecurityToken sec) m ()
auth !token !scope =
  tell' $ Tries mempty
                mempty
                (singleton origin_ $ SecurityToken token scope)


{-# INLINEABLE auth #-}


-- * Routing ---------------------------------------

-- | Use this function to run your 'RouterT' into a 'MiddlewareT';
--   making your router executable in WAI. Note that this only
--   responds with content, and doesn't protect your routes with
--   your calls to 'auth'; to protect routes, postcompose this
--   with 'routeAuth':
--
--   > route routes . routeAuth routes
route :: ( MonadIO m
         ) => RouterT (MiddlewareT m) sec m a -- ^ The Router
           -> MiddlewareT m
route hs app req resp = do
  let path = pathInfo req
  mightMatch <- extractMatch path hs
  case mightMatch of
    Nothing -> do
      mMatch <- extractMatchAny path hs
      maybe
        (app req resp)
        (\mid -> mid app req resp)
        mMatch
    Just mid -> mid app req resp


-- | Supply a method to decide whether or not to 'Control.Monad.Catch.throwM'
--   an exception based on the current 'Network.Wai.Middleware.Request' and
--   the /layers/ of 'auth' tokens passed in your router, turn your router
--   into a 'Control.Monad.guard' for middlewares, basically.
routeAuth :: ( MonadIO m
             , MonadThrow m
             ) => (Request -> [sec] -> m ()) -- ^ authorization method
               -> RouterT (MiddlewareT m) (SecurityToken sec) m a -- ^ The Router
               -> MiddlewareT m
routeAuth authorize hs app req resp = do
  extractAuth authorize req hs
  route hs app req resp

-- * Extraction -------------------------------

-- | Extracts only the normal 'match', 'matchGroup' and 'matchHere' routes.
extractMatch :: ( MonadIO m
                ) => [T.Text] -- ^ The path to match against
                  -> RouterT r sec m a -- ^ The Router
                  -> m (Maybe r)
extractMatch path !hs = do
  Tries{trieContent} <- execRouterT hs
  let mResult = lookupWithLRPT trimFileExt path trieContent
  case mResult of
    Nothing ->
      if not (null path)
         && trimFileExt (last path) == "index"
      then pure $ Interface.lookup (init path) trieContent
      else pure Nothing
    Just (_,r) -> pure (Just r)

{-# INLINEABLE extractMatch #-}


-- | Extracts only the 'matchAny' responses; something like the greatest-lower-bound.
extractMatchAny :: ( MonadIO m
                   ) => [T.Text] -- ^ The path to match against
                     -> RouterT r sec m a -- ^ The Router
                     -> m (Maybe r)
extractMatchAny path = extractNearestVia path (\x -> trieCatchAll <$> execRouterT x)

{-# INLINEABLE extractMatchAny #-}



-- | Find the security tokens / authorization roles affiliated with
--   a request for a set of routes.
extractAuthSym :: ( MonadIO m
                  ) => [T.Text] -- ^ The path to match against
                    -> RouterT x (SecurityToken sec) m a -- ^ The Router
                    -> m [sec]
extractAuthSym path hs = do
  Tries{trieSecurity} <- execRouterT hs
  liftIO . stToIO $ do
    let results = Interface.matches path trieSecurity
    pure $! foldr go [] results
  where
    go (_,SecurityToken _ DontProtectHere,[]) ys = ys
    go (_,SecurityToken x _              ,_ ) ys = x:ys

{-# INLINEABLE extractAuthSym #-}

-- | Extracts only the security handling logic, and turns it into a guard.
extractAuth :: ( MonadIO m
               , MonadThrow m
               ) => (Request -> [sec] -> m ()) -- ^ authorization method
                 -> Request
                 -> RouterT x (SecurityToken sec) m a
                 -> m ()
extractAuth authorize req hs = do
  ss <- extractAuthSym (pathInfo req) hs
  authorize req ss

{-# INLINEABLE extractAuth #-}


-- | Given a way to draw out a special-purpose trie from our route set, route
--   to the responses based on a /furthest-route-reached/ method, or like a
--   greatest-lower-bound.
extractNearestVia :: ( MonadIO m
                     ) => [T.Text] -- ^ The path to match against
                       -> (RouterT r sec m a -> m (RootedPredTrie T.Text r))
                       -> RouterT r sec m a
                       -> m (Maybe r)
extractNearestVia path extr hs = do
  trie <- extr hs
  pure (mid <$> Interface.match path trie)
  where
    mid (_,r,_) = r

{-# INLINEABLE extractNearestVia #-}



-- * Pred-Trie related -----------------

-- | Removes @.txt@ from @foo.txt@
trimFileExt :: T.Text -> T.Text
trimFileExt !s =
  case T.breakOnEnd "." s of
    (f,e) | f /= ""
          && e /= ""
          && T.length f > 0 -> T.dropEnd 1 f
    _ -> s

{-# INLINEABLE trimFileExt #-}


-- | A quirky function for processing the last element of a lookup path, only
-- on /literal/ matches.
lookupWithLPT :: ( Hashable s
                 , Eq s
                 ) => (s -> s) -> NonEmpty s -> PredTrie s a -> Maybe ([s], a)
lookupWithLPT f tss (PredTrie (HashMapStep ls) (PredSteps ps)) =
  getFirst $ First (goLit f tss ls)
          <> foldMap (First . goPred f tss) ps

goLit :: ( Hashable s
         , Eq s
         ) => (s -> s)
           -> NonEmpty s
           -> HM.HashMap s (HashMapChildren PredTrie s a)
           -> Maybe ([s], a)
goLit f (t:|ts) xs = do
  (HashMapChildren mx mxs) <- getFirst $ First (HM.lookup t xs)
                                      <> First (  if null ts
                                                  then HM.lookup (f t) xs
                                                  else Nothing)
  if null ts
  then ([f t],) <$> mx
  else first (t:) <$> (lookupWithLPT f (fromList ts) =<< mxs)

goPred :: ( Hashable s
          , Eq s
          ) => (s -> s)
            -> NonEmpty s
            -> PredStep s PredTrie s a
            -> Maybe ([s], a)
goPred f (t:|ts) (PredStep _ predicate mx xs) = do
  d <- predicate t
  if null ts
  then (([t],) . ($ d)) <$> mx
  else bimap (t:) ($ d) <$> lookupWithLPT f (fromList ts) xs

-- lookupWithLPT :: ( Eq k
--                  , Hashable k
--                  , Typeable s
--                  , Typeable k
--                  ) => PredSet s k
--                    -> (k -> k)
--                    -> NonEmpty k
--                    -> PredTrie k a
--                    -> ST s (Maybe a)
-- lookupWithLPT predSet f (k:|ks) (HashTableTrie raw preds) = do
--   mx <- HT.lookup raw $ if null ks then f k else k
--   case mx of
--     Just (RawValue mx' children) ->
--       case ks of
--         []       -> pure mx'
--         (k':ks') -> lookupWithLPT predSet f (k':|ks') children
--     Nothing ->
--       let -- go :: Typeable t => Maybe t -> PredStep s k t -> ST s (Maybe t)
--           go solution@(Just _) _                          = pure solution
--           go Nothing (MPT.PredStep predKey mHandler children) = do
--             mx' <- HS.lookup predKey k predSet
--             case mx' of
--               Nothing -> pure Nothing
--               Just x  ->
--                 case ks of
--                   [] ->
--                     pure $! ($ x) <$> mHandler
--                   (k':ks') -> do
--                     mf <- lookupWithLPT predSet f (k':|ks') children
--                     pure $! ($ x) <$> mf
--       in  foldlM go Nothing preds

-- lookupWithLPT :: ( Eq k
--                 , Hashable k
--                 , Typeable s
--                 , Typeable k
--                 ) => PredSet s k
--                   -> (k -> k)
--                   -> NonEmpty k
--                   -> HashTableTrie s k a
--                   -> ST s (Maybe (NonEmpty k, a, [k]))
-- lookupWithLPT predSet f (k:|ks) (HashTableTrie raw preds) = do
--   mLit <- goLit raw
--   case mLit of
--     Just _  -> pure mLit
--     Nothing ->
--       let go solution@(Just _) _ = pure solution
--           go Nothing pred        = goPred pred
--       in  foldlM go Nothing preds
--   where
--     goLit xs = do
--       mx' <- if null ks
--              then HT.lookup raw (f k)
--              else HT.lookup raw k
--       case mx' of
--         Nothing -> pure Nothing
--         Just (RawValue mx children) ->
--           let mFoundHere = (\x -> (k:|[], x, ks)) <$> mx
--               prependAncestry (pre,x,suff) = (k:|NE.toList pre,x,suff)
--           in case ks of
--             [] -> pure mFoundHere
--             (k':ks') -> do
--               mFoundThere <- MPT.match predSet (k':|ks') children
--               pure $! getFirst $
--                    First (prependAncestry <$> mFoundThere)
--                 <> First mFoundHere
--
--     goPred (MPT.PredStep predKey mx children) = do
--       mr' <- HS.lookup predKey k predSet
--       case mr' of
--         Nothing -> pure Nothing
--         Just r  ->
--           let mFoundHere = (\x -> (k:|[], x r, ks)) <$> mx
--               prependAncestryAndApply (pre,f,suff) =
--                 (k:|NE.toList pre,f r,suff)
--           in case ks of
--             [] -> pure mFoundHere
--             (k':ks') -> do
--               mFoundThere <- MPT.match predSet (k':|ks') children
--               pure $! getFirst $
--                    First (prependAncestryAndApply <$> mFoundThere)
--                 <> First mFoundHere



{-# INLINEABLE lookupWithLPT #-}


lookupWithLRPT :: ( Hashable s
                 , Eq s
                 ) => (s -> s) -> [s] -> RootedPredTrie s a -> Maybe ([s], a)
lookupWithLRPT _ [] (RootedPredTrie mx _) = ([],) <$> mx
lookupWithLRPT f ts (RootedPredTrie _ xs) = lookupWithLPT f (fromList ts) xs

-- lookupWithLRPT :: ( Eq k
--                  , Hashable k
--                  , Typeable s
--                  , Typeable k
--                  , Typeable a
--                  ) => (k -> k)
--                    -> [k]
--                    -> RootedHashTableTrie s k a
--                    -> ST s (Maybe ([k],a,[k]))
-- lookupWithLRPT _ [] (RootedHashTableTrie mx _ _) =
--   pure $! (\x -> ([],x,[])) <$> mx
-- lookupWithLRPT f (k:ks) (RootedHashTableTrie mx xs predSet) = do
--   mFoundThere <- lookupWithLPT predSet f (k:|ks) xs
--   pure $! getFirst $
--       First ((\(pre,x,suff) -> (NE.toList pre,x,suff)) <$> mFoundThere)
--    <> First ((\x -> ([],x,k:ks)) <$> mx)

-- lookupWithLRPT :: ( Eq k
--                   , Hashable k
--                   , Typeable s
--                   , Typeable k
--                   , Typeable a
--                   ) => (k -> k)
--                     -> [k]
--                     -> RootedHashTableTrie s k a
--                     -> ST s (Maybe a)
-- lookupWithLRPT _ [] (RootedHashTableTrie mx _ _) = pure mx
-- lookupWithLRPT f (k:ks) (RootedHashTableTrie _ xs predSet) =
--   lookupWithLPT predSet f (k:|ks) xs



{-# INLINEABLE lookupWithLRPT #-}


tell' :: (Monoid w, S.MonadState w m) => w -> m ()
tell' x = S.modify' (<> x)

{-# INLINEABLE tell' #-}

