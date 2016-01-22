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
import           Data.Trie.Pred.Base                (RootedPredTrie (..), PredTrie (..))
import           Data.Trie.Pred.Interface.Types     (Singleton (..), Extrude (..), CatMaybes)

import           Data.Monoid
import qualified Data.Text as T
import           Data.Function.Poly
import           Control.Monad.Trans
import qualified Control.Monad.State                as S
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch



data Tries x s = Tries
  { trieContent  :: !(RootedPredTrie T.Text x)
  , trieCatchAll :: !(RootedPredTrie T.Text x)
  , trieSecurity :: !(RootedPredTrie T.Text s)
  }

instance Monoid (Tries x s) where
  mempty = Tries mempty mempty mempty
  mappend (Tries x1 x2 x3) (Tries y1 y2 y3) =
    ((Tries $! x1 <> y1)
            $! x2 <> y2)
            $! x3 <> y3

-- | Will have a shape of @HandlerT (MiddlewareT m) (SecurityToken s) m a@
--   when used.
newtype HandlerT x sec m a = HandlerT
  { runHandlerT :: S.StateT (Tries x sec) m a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadTrans
             , S.MonadState (Tries x sec))

execHandlerT :: Monad m => HandlerT x sec m a -> m (Tries x sec)
execHandlerT hs = S.execStateT (runHandlerT hs) mempty

{-# INLINEABLE execHandlerT #-}

type ExtrudeSoundly cleanxs xs c r =
  ( cleanxs ~ CatMaybes xs
  , ArityTypeListIso c cleanxs r
  , Extrude (UrlChunks xs)
      (RootedPredTrie T.Text c)
      (RootedPredTrie T.Text r)
  )


type ActionT m a = VerbListenerT (FileExtListenerT Response m a) m a

action :: Monad m => ActionT m () -> MiddlewareT m
action xs app req respond = do
  vmap <- execVerbListenerT (mapVerbs fileExtsToMiddleware xs)
  let v = getVerb req
  mMid <- lookupVerb req v vmap
  case mMid of
    Nothing  -> app req respond
    Just mid -> mid app req respond
