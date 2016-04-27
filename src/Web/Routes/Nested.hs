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
you to tap into the monad transformer stack for logging, STM variables, database queries,
etc.
-}


module Web.Routes.Nested
  ( -- * Combinators
    match
  , matchHere
  , matchAny
  , matchGroup
  , auth
  , -- * Routing
    route
  , routeAuth
  , extractMatch
  , extractMatchAny
  , extractAuthSym
  , extractAuth
  , extractNearestVia
  , -- * Metadata
    SecurityToken (..)
  , AuthScope (..)
  , -- * Re-Exports
    module Web.Routes.Nested.Match
  , module Web.Routes.Nested.Types
  , module Network.Wai.Middleware.Verbs
  , module Network.Wai.Middleware.ContentType
  ) where

import           Web.Routes.Nested.Match
import           Web.Routes.Nested.Types
import           Network.Wai.Trans
import           Network.Wai.Middleware.Verbs
import           Network.Wai.Middleware.ContentType hiding (responseStatus, responseHeaders, responseData)

import Data.Foldable (foldlM)
import qualified Data.HashTable.ST.Basic            as HT
import           Data.PredSet.Mutable               as HS
import           Data.Trie.Pred.Mutable             (HashTableTrie (..), RootedHashTableTrie (..), RawValue (..))
import qualified Data.Trie.Pred.Mutable             as MPT
import           Data.Trie.Pred.Mutable.Morph       (toMutableRooted)
import qualified Data.Trie.Pred.Base                as PT -- only using lookups
import           Data.Trie.Pred.Base                (RootedPredTrie (..), PredTrie (..))
import           Data.Trie.Pred.Base.Step           (PredStep (..), PredSteps (..))
import           Data.Trie.Pred.Interface.Types     (Singleton (..), Extrude (..), CatMaybes)
import qualified Data.Trie.Class                    as TC
import           Data.Trie.HashMap                  (HashMapStep (..), HashMapChildren (..))
import qualified Data.HashMap.Lazy                  as HM
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import           Data.Hashable
import           Data.Monoid
import           Data.Functor.Syntax
import           Data.Function.Poly
import Data.Typeable

import           Control.Monad
import qualified Control.Monad.State                as S
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.ST


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
         , Singleton (UrlChunks xs)
             childContent
             (RootedPredTrie T.Text resultContent)
         , cleanxs ~ CatMaybes xs
         , ArityTypeListIso childContent cleanxs resultContent
         ) => UrlChunks xs
           -> childContent
           -> HandlerT resultContent sec m ()
match !ts !vl =
  tell' $ Tries (singleton ts vl)
                mempty
                mempty


{-# INLINEABLE match #-}

-- | Create a handle for the /current/ route - an alias for @\h -> match o_ h@.
matchHere :: ( Monad m
             ) => content
               -> HandlerT content sec m ()
matchHere = match origin_

{-# INLINEABLE matchHere #-}


-- | Match against any route, as a last resort against all failing matches -
--   use this for a catch-all at some level in their routes, something
--   like a @not-found 404@ page is useful.
matchAny :: ( Monad m
            ) => content
              -> HandlerT content sec m ()
matchAny !vl =
  tell' $ Tries mempty
                (singleton origin_ vl)
                mempty


{-# INLINEABLE matchAny #-}


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
matchGroup !ts cs = do
  (Tries trieContent trieNotFound trieSec) <- lift $ execHandlerT cs
  tell' $ Tries (extrude ts trieContent)
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
        ) => sec
          -> AuthScope
          -> HandlerT content (SecurityToken sec) m ()
auth !token !scope =
  tell' $ Tries mempty
                mempty
                (singleton origin_ $ SecurityToken token scope)


{-# INLINEABLE auth #-}


-- * Routing ---------------------------------------

route :: ( Monad m
         , MonadIO m
         , Typeable m
         ) => HandlerT (MiddlewareT m) sec m a
           -> MiddlewareT m
route hs app req resp = do
  let path = pathInfo req
  mMatch <- extractMatch path hs
  case mMatch of
    Nothing -> do
      mMatch <- extractMatchAny path hs
      maybe
        (app req resp)
        (\mid -> mid app req resp)
        mMatch
    Just mid -> mid app req resp

routeAuth :: ( Monad m
             , MonadIO m
             , MonadThrow m
             , Typeable sec
             , Typeable m
             ) => (Request -> [sec] -> m ())
               -> HandlerT (MiddlewareT m) (SecurityToken sec) m a
               -> MiddlewareT m
routeAuth authorize hs app req resp = do
  extractAuth authorize req hs
  route hs app req resp

-- * Extraction -------------------------------

-- | Extracts only the normal 'match' and 'matchHere'
extractMatch :: ( Monad m
                , MonadIO m
                , Typeable r
                ) => [T.Text]
                  -> HandlerT r sec m a
                  -> m (Maybe r)
extractMatch path !hs = do
  tries <- execHandlerT hs
  liftIO $ stToIO $ do
    trie <- trieContentMutable tries
    mResult <- matchWithLRPT trimFileExt path trie
    case mResult of
      Nothing ->
        if not (null path)
           && trimFileExt (last path) == "index"
        then MPT.lookupR (init path) trie
        else pure Nothing
      Just (_,r,_) -> return (Just r)

{-# INLINEABLE extractMatch #-}


-- | Extracts only the @notFound@ responses
extractMatchAny :: ( Monad m
                   , MonadIO m
                   , Typeable r
                   ) => [T.Text]
                     -> HandlerT r sec m a
                     -> m (Maybe r)
extractMatchAny path = extractNearestVia path (\x -> trieCatchAll <$> execHandlerT x)

{-# INLINEABLE extractMatchAny #-}



-- | Find the security tokens / authorization roles affiliated with
--   a request for a set of routes.
extractAuthSym :: ( Monad m
                  , MonadIO m
                  , Typeable sec
                  ) => [T.Text]
                    -> HandlerT x (SecurityToken sec) m a
                    -> m [sec]
extractAuthSym path hs = do
  tries <- execHandlerT hs
  liftIO . stToIO $ do
    results <- MPT.matchesR path =<< trieSecurityMutable tries
    pure $! foldr go [] results
  where
    go (_,SecurityToken _ DontProtectHere,[]) ys = ys
    go (_,SecurityToken x _              ,_ ) ys = x:ys

{-# INLINEABLE extractAuthSym #-}

-- | Extracts only the security handling logic, and turns it into a guard
extractAuth :: ( Monad m
               , MonadIO m
               , MonadThrow m
               , Typeable sec
               ) => (Request -> [sec] -> m ()) -- authorization method
                 -> Request
                 -> HandlerT x (SecurityToken sec) m a
                 -> m ()
extractAuth authorize req hs = do
  ss <- extractAuthSym (pathInfo req) hs
  authorize req ss

{-# INLINEABLE extractAuth #-}


-- | Given a way to draw out a special-purpose trie from our route set, route
--   to the responses based on a /furthest-reached/ method.
extractNearestVia :: ( MonadIO m
                     , Monad m
                     , Typeable r
                     ) => [T.Text]
                       -> (HandlerT r sec m a -> m (RootedPredTrie T.Text r))
                       -> HandlerT r sec m a
                       -> m (Maybe r)
extractNearestVia path extr hs = do
  trie <- extr hs
  liftIO . stToIO $ do
    mResult <- MPT.matchR path =<< toMutableRooted trie
    pure (mid <$> mResult)
  where
    mid (_,r,_) = r

{-# INLINEABLE extractNearestVia #-}



-- * Pred-Trie related -----------------

-- | Removes @.txt@ from @foo.txt@
trimFileExt :: T.Text -> T.Text
trimFileExt !s = fst $! T.breakOn "." s

{-# INLINEABLE trimFileExt #-}


-- | A quirky function for processing the last element of a lookup path, only
-- on /literal/ matches.
-- matchWithLPT :: ( Hashable s
--                 , Eq s
--                 ) => (s -> s) -> NonEmpty s -> PredTrie s a -> Maybe ([s], a)
-- matchWithLPT f (t:|ts) (PredTrie (HashMapStep ls) (PredSteps ps))
--   | null ts   = getFirst $ First ((goLit $! f t) ls) <> foldMap (First . goPred) ps
--   | otherwise = getFirst $ First (goLit       t  ls) <> foldMap (First . goPred) ps
--   where
--     goLit t' xs = do
--       (HashMapChildren mx mxs) <- HM.lookup t' xs
--       if null ts
--       then ([t],) <$> mx
--       else fmap (\(ts',x) -> (t:ts',x)) $! matchWithLPT f (NE.fromList ts) =<< mxs
-- 
--     goPred (PredStep _ predicate mx xs) = do
--       d <- predicate t
--       if null ts
--       then ([t],) <$> (mx <$~> d)
--       else fmap (\(ts',x) -> (t:ts',x d)) $! matchWithLPT f (NE.fromList ts) xs

matchWithLPT :: ( Eq k
                , Hashable k
                , Typeable s
                , Typeable k
                ) => PredSet s k
                  -> (k -> k)
                  -> NonEmpty k
                  -> HashTableTrie s k a
                  -> ST s (Maybe (NonEmpty k, a, [k]))
matchWithLPT predSet f (k:|ks) (HashTableTrie raw preds) = do
  mLit <- goLit raw
  case mLit of
    Just _  -> pure mLit
    Nothing ->
      let go solution@(Just _) _ = pure solution
          go Nothing pred        = goPred pred
      in  foldlM go Nothing preds
  where
    goLit xs = do
      mx' <- if null ks
             then HT.lookup raw (f k)
             else HT.lookup raw k
      case mx' of
        Nothing -> pure Nothing
        Just (RawValue mx children) ->
          let mFoundHere = (\x -> (k:|[], x, ks)) <$> mx
              prependAncestry (pre,x,suff) = (k:|NE.toList pre,x,suff)
          in case ks of
            [] -> pure mFoundHere
            (k':ks') -> do
              mFoundThere <- MPT.match predSet (k':|ks') children
              pure $! getFirst $
                   First (prependAncestry <$> mFoundThere)
                <> First mFoundHere

    goPred (MPT.PredStep predKey mx children) = do
      mr' <- HS.lookup predKey k predSet
      case mr' of
        Nothing -> pure Nothing
        Just r  ->
          let mFoundHere = (\x -> (k:|[], x r, ks)) <$> mx
              prependAncestryAndApply (pre,f,suff) =
                (k:|NE.toList pre,f r,suff)
          in case ks of
            [] -> pure mFoundHere
            (k':ks') -> do
              mFoundThere <- MPT.match predSet (k':|ks') children
              pure $! getFirst $
                   First (prependAncestryAndApply <$> mFoundThere)
                <> First mFoundHere



{-# INLINEABLE matchWithLPT #-}


--matchWithLRPT :: ( Hashable s
--                 , Eq s
--                 ) => (s -> s) -> [s] -> RootedPredTrie s a -> Maybe ([s], a)
--matchWithLRPT _ [] (RootedPredTrie mx _) = ([],) <$> mx
--matchWithLRPT f ts (RootedPredTrie _ xs) = matchWithLPT f (NE.fromList ts) xs

matchWithLRPT :: ( Eq k
                 , Hashable k
                 , Typeable s
                 , Typeable k
                 , Typeable a
                 ) => (k -> k)
                   -> [k]
                   -> RootedHashTableTrie s k a
                   -> ST s (Maybe ([k],a,[k]))
matchWithLRPT _ [] (RootedHashTableTrie mx _ _) =
  pure $! (\x -> ([],x,[])) <$> mx
matchWithLRPT f (k:ks) (RootedHashTableTrie mx xs predSet) = do
  mFoundThere <- matchWithLPT predSet f (k:|ks) xs
  pure $! getFirst $
      First ((\(pre,x,suff) -> (NE.toList pre,x,suff)) <$> mFoundThere)
   <> First ((\x -> ([],x,k:ks)) <$> mx)



{-# INLINEABLE matchWithLRPT #-}


tell' :: (Monoid w, S.MonadState w m) => w -> m ()
tell' x = S.modify' (<> x)

{-# INLINEABLE tell' #-}

