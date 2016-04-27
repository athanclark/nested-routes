{-# LANGUAGE
    DeriveFunctor
  , ConstraintKinds
  , TypeFamilies
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}

module Web.Routes.Nested.Types where

import           Web.Routes.Nested.Match
import           Network.Wai.Middleware.Verbs
import           Network.Wai.Middleware.ContentType
import           Network.Wai.Trans
import           Data.Trie.Pred.Mutable
import           Data.Trie.Pred.Mutable.Morph
import           Data.Trie.Pred.Base                (RootedPredTrie (..))
import           Data.Trie.Pred.Interface.Types     (Extrude (..), CatMaybes)

import Data.Typeable
import           Data.Monoid
import qualified Data.Text as T
import           Data.Function.Poly
import           Control.Monad.Trans
import qualified Control.Monad.State                as S

import Control.Monad.ST


-- | The internal data structure built during route declaration.
data Tries x s = Tries
  { trieContent  :: !(RootedPredTrie T.Text x)
  , trieCatchAll :: !(RootedPredTrie T.Text x)
  , trieSecurity :: !(RootedPredTrie T.Text s)
  }

trieContentMutable :: ( Typeable x
                      , Typeable s
                      ) => Tries x s'
                        -> ST s (RootedHashTableTrie s T.Text x)
trieContentMutable (Tries x _ _) = toMutableRooted x

trieCatchAllMutable :: ( Typeable x
                       , Typeable s
                       ) => Tries x s'
                         -> ST s (RootedHashTableTrie s T.Text x)
trieCatchAllMutable (Tries _ x _) = toMutableRooted x

trieSecurityMutable :: ( Typeable s'
                       , Typeable s
                       ) => Tries x s'
                         -> ST s (RootedHashTableTrie s T.Text s')
trieSecurityMutable (Tries _ _ x) = toMutableRooted x

instance Monoid (Tries x s) where
  mempty = Tries mempty mempty mempty
  mappend (Tries x1 x2 x3) (Tries y1 y2 y3) =
    ((Tries $! x1 <> y1)
            $! x2 <> y2)
            $! x3 <> y3

-- | The return type of a route building expression like `match` -
--   it should have a shape of @HandlerT (MiddlewareT m) (SecurityToken s) m a@
--   when used with `route`.
newtype HandlerT x sec m a = HandlerT
  { runHandlerT :: S.StateT (Tries x sec) m a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadTrans
             , S.MonadState (Tries x sec))

-- | Run the monad, only getting the built state and throwing away @a@.
execHandlerT :: Monad m => HandlerT x sec m a -> m (Tries x sec)
execHandlerT hs = S.execStateT (runHandlerT hs) mempty

{-# INLINEABLE execHandlerT #-}

-- | Deductive proof that prepending a list of types to a function as arity
--   can be deconstructed.
type ExtrudeSoundly cleanxs xs c r =
  ( cleanxs ~ CatMaybes xs
  , ArityTypeListIso c cleanxs r
  , Extrude (UrlChunks xs)
      (RootedPredTrie T.Text c)
      (RootedPredTrie T.Text r)
  )

-- | The type of content builders.
type ActionT m a = VerbListenerT (FileExtListenerT m a) m a

-- | Run the content builder into a middleware that responds when the content
--   is satisfiable (i.e. @Accept@ headers are O.K., etc.)
action :: Monad m => ActionT m () -> MiddlewareT m
action xs app req respond = do
  vmap <- execVerbListenerT (mapVerbs fileExtsToMiddleware xs)
  let v = getVerb req
  mMid <- lookupVerb req v vmap
  case mMid of
    Nothing  -> app req respond
    Just mid -> mid app req respond
