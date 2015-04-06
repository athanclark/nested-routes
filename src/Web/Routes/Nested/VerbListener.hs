{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Routes.Nested.VerbListener where

import           Web.Routes.Nested.FileExtListener

import           Network.Wai

import           Control.Applicative hiding (empty)
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Foldable
import           Data.Traversable
import           Data.Map.Lazy
import qualified Data.ByteString.Lazy                 as BL


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

newtype Verbs z m r = Verbs { unVerbs :: Map Verb (Maybe (ReaderT BL.ByteString m z), FileExts r) }
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


get :: (Monad m) =>
       FileExtListenerT Response m a
    -> VerbListenerT z Response m ()
get flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Get (Nothing, fileexts)
  VerbListenerT $ tell $ Verbs new


post :: (Monad m, MonadIO m) =>
        (BL.ByteString -> m z)
     -> FileExtListenerT Response m a
     -> VerbListenerT z Response m ()
post handle flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Post (Just $ ReaderT handle, fileexts)
  VerbListenerT $ tell $ Verbs new


put :: (Monad m, MonadIO m) =>
       (BL.ByteString -> m z)
    -> FileExtListenerT Response m a
    -> VerbListenerT z Response m ()
put handle flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Put (Just $ ReaderT handle, fileexts)
  VerbListenerT $ tell $ Verbs new


delete :: (Monad m) =>
          FileExtListenerT Response m a
       -> VerbListenerT z Response m ()
delete flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Delete (Nothing, fileexts)
  VerbListenerT $ tell $ Verbs new
