{-# LANGUAGE
    DeriveFunctor
  , ConstraintKinds
  , TypeFamilies
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}

{- |
Module      : Web.Routes.Nested.Types
Copyright   : (c) 2015, 2016, 2017, 2018 Athan Clark

License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Web.Routes.Nested.Types
  ( -- * Internal Structure
    Tries (..)
  , -- * Builder
    RouterT (..)
  , execRouterT
  , ActionT
  , action
  , -- * Book Keeping
    ExtrudeSoundly
  ) where

import           Web.Routes.Nested.Match             (UrlChunks)
import           Network.Wai.Middleware.Verbs        (VerbListenerT, execVerbListenerT, getVerbFromRequest)
import           Network.Wai.Middleware.ContentType  (FileExtListenerT, fileExtsToMiddleware)
import           Network.Wai.Trans                   (MiddlewareT)
import           Network.Wai                         (strictRequestBody)
import           Data.Trie.Pred.Base                 (RootedPredTrie (..))
import           Data.Trie.Pred.Interface.Types      (Extrude (..), CatMaybes)

import           Data.Monoid                         ((<>))
import qualified Data.Text                           as T
import           Data.Function.Poly                  (ArityTypeListIso)
import           Data.Singleton.Class                (Extractable)
import qualified Data.HashMap.Lazy                   as HM
import           Control.Monad.Trans                 (MonadTrans)
import           Control.Monad.IO.Class              (MonadIO)
import qualified Control.Monad.State                 as S
import           Control.Monad.Trans.Control.Aligned (MonadBaseControl (liftBaseWith))



-- | The internal data structure built during route declaration.
data Tries x s = Tries
  { trieContent  :: !(RootedPredTrie T.Text x)
  , trieCatchAll :: !(RootedPredTrie T.Text x)
  , trieSecurity :: !(RootedPredTrie T.Text s)
  }


instance Monoid (Tries x s) where
  mempty = Tries mempty mempty mempty
  mappend (Tries x1 x2 x3) (Tries y1 y2 y3) =
    Tries (x1 <> y1) (x2 <> y2) (x3 <> y3)

-- | The (syntactic) monad for building a router with functions like
--   "Web.Routes.Nested.match".
--   it should have a shape of @RouterT (MiddlewareT m) (SecurityToken s) m a@
--   when used with "Web.Routes.Nested.route".
newtype RouterT x sec m a = RouterT
  { runRouterT :: S.StateT (Tries x sec) m a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadTrans
             , S.MonadState (Tries x sec))

-- | Run the monad, only getting the built state and throwing away @a@.
execRouterT :: Monad m => RouterT x sec m a -> m (Tries x sec)
execRouterT hs = S.execStateT (runRouterT hs) mempty

{-# INLINEABLE execRouterT #-}

-- | Soundness constraint showing that a function's arity
--   can be represented as a type-level list.
type ExtrudeSoundly xs' xs c r =
  ( xs' ~ CatMaybes xs
  , ArityTypeListIso c xs' r
  , Extrude (UrlChunks xs)
      (RootedPredTrie T.Text c)
      (RootedPredTrie T.Text r)
  )


-- | The type of "content" builders; using the
--   <https://hackage.haskell.org/package/wai-middleware-verbs wai-middleware-verbs>
--   and <https://hackage.haskell.org/package/wai-middleware-content-type wai-middleware-content-type>
--   packages.
type ActionT m a = VerbListenerT (FileExtListenerT m a) m a

-- | Run the content builder into a middleware that responds when the content
--   is satisfiable (i.e. @Accept@ headers are O.K., etc.)
action :: MonadBaseControl IO m stM
       => Extractable stM
       => ActionT m ()
       -> MiddlewareT m
action xs app req respond = do
  vmap <- execVerbListenerT xs
  case HM.lookup (getVerbFromRequest req) vmap of
    Nothing -> app req respond
    Just eR -> do
      c <- case eR of
        Left c' -> pure c'
        Right f -> f <$> liftBaseWith (\_ -> strictRequestBody req)
      fileExtsToMiddleware c app req respond
