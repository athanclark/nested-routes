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

newtype Verbs z m r = Verbs { unVerbs :: Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), r) }
  deriving (Functor, Traversable)

deriving instance                  Monoid    (Verbs z m a)
deriving instance                  Foldable  (Verbs z m)

newtype VerbListenerT z r m a =
  VerbListenerT { runVerbListenerT :: WriterT (Verbs z m r) m a }
    deriving (Functor)

deriving instance Applicative m => Applicative (VerbListenerT z r m)
deriving instance Monad m =>       Monad       (VerbListenerT z r m)
deriving instance MonadIO m =>     MonadIO     (VerbListenerT z r m)
instance MonadTrans (VerbListenerT z r) where
  lift ma = VerbListenerT $ lift ma


foldMWithKey :: Monad m => (acc -> Verb -> a -> m acc) -> acc -> Map Verb a -> m acc
foldMWithKey f i = foldlWithKey (\macc k a -> (\mer -> f mer k a) =<< macc) (return i)


get :: (Monad m) =>
       a
    -> VerbListenerT z a m ()
get r = do
  let new = singleton Get (Nothing, r)
  VerbListenerT $ tell $ Verbs new


post :: (Monad m, MonadIO m) =>
        (BL.ByteString -> m z)
     -> a
     -> VerbListenerT z a m ()
post handle r = do
  let new = singleton Post (Just (ReaderT handle, Nothing), r)
  VerbListenerT $ tell $ Verbs new


postMax :: (Monad m, MonadIO m) =>
           BodyLength
        -> (BL.ByteString -> m z)
        -> a
        -> VerbListenerT z a m ()
postMax bl handle r = do
  let new = singleton Post (Just (ReaderT handle, Just bl), r)
  VerbListenerT $ tell $ Verbs new


put :: (Monad m, MonadIO m) =>
       (BL.ByteString -> m z)
    -> a
    -> VerbListenerT z a m ()
put handle r = do
  let new = singleton Put (Just (ReaderT handle, Nothing), r)
  VerbListenerT $ tell $ Verbs new


putMax :: (Monad m, MonadIO m) =>
          BodyLength
       -> (BL.ByteString -> m z)
       -> a
       -> VerbListenerT z a m ()
putMax bl handle r = do
  let new = singleton Put (Just (ReaderT handle, Just bl), r)
  VerbListenerT $ tell $ Verbs new


delete :: (Monad m) =>
          a
       -> VerbListenerT z a m ()
delete r = do
  let new = singleton Delete (Nothing, r)
  VerbListenerT $ tell $ Verbs new
