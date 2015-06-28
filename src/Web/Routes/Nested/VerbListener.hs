{-# LANGUAGE
    DeriveFunctor
  , DeriveTraversable
  , DeriveFoldable
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , StandaloneDeriving
  , MultiParamTypeClasses
  #-}

module Web.Routes.Nested.VerbListener where

import           Control.Applicative hiding (empty)
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Traversable
import           Data.Map.Lazy
import qualified Data.ByteString.Lazy                 as BL
import           Data.Word                            (Word64)


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

type BodyLength = Word64

newtype Verbs m r = Verbs { unVerbs :: Map Verb (Maybe (BL.ByteString -> m (), Maybe BodyLength), r) }
  deriving (Monoid, Functor, Foldable, Traversable)

newtype VerbListenerT r m a =
  VerbListenerT { runVerbListenerT :: WriterT (Verbs m r) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (VerbListenerT r) where
  lift ma = VerbListenerT $ lift ma


foldMWithKey :: Monad m => (acc -> Verb -> a -> m acc) -> acc -> Map Verb a -> m acc
foldMWithKey f i = foldlWithKey (\macc k a -> (\mer -> f mer k a) =<< macc) (return i)


get :: ( Monad m
       ) => r -> VerbListenerT r m ()
get r = do
  let new = singleton Get (Nothing, r)
  VerbListenerT $ tell $ Verbs new


post :: ( Monad m
        , MonadIO m
        ) => (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
post handle r = do
  let new = singleton Post (Just (handle, Nothing), r)
  VerbListenerT $ tell $ Verbs new


postMax :: ( Monad m
           , MonadIO m
           ) => BodyLength -> (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
postMax bl handle r = do
  let new = singleton Post (Just (handle, Just bl), r)
  VerbListenerT $ tell $ Verbs new


put :: ( Monad m
       , MonadIO m
       ) => (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
put handle r = do
  let new = singleton Put (Just (handle, Nothing), r)
  VerbListenerT $ tell $ Verbs new


putMax :: ( Monad m
          , MonadIO m
          ) => BodyLength -> (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
putMax bl handle r = do
  let new = singleton Put (Just (handle, Just bl), r)
  VerbListenerT $ tell $ Verbs new


delete :: ( Monad m
          ) => r -> VerbListenerT r m ()
delete r = do
  let new = singleton Delete (Nothing, r)
  VerbListenerT $ tell $ Verbs new
