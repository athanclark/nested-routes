{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Routes.Nested.VerbListener where

import           Web.Routes.Nested.FileExtListener

import           Network.Wai

import           Control.Applicative hiding (empty)
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Monoid
import           Data.Foldable
import           Data.Traversable
import           Data.Map.Lazy


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

newtype Verbs a = Verbs { unVerbs :: Map (FileExt, Verb) a }
  deriving (Show, Eq, Functor, Traversable)

deriving instance Monoid      (Verbs a)
deriving instance Foldable     Verbs


newtype VerbListenerT r m a =
  VerbListenerT { runVerbListenerT :: WriterT (Verbs r) m a }
    deriving (Functor)

deriving instance Applicative m => Applicative (VerbListenerT r m)
deriving instance Monad m =>       Monad       (VerbListenerT r m)
deriving instance MonadIO m =>     MonadIO     (VerbListenerT r m)
deriving instance                  MonadTrans  (VerbListenerT r)


get :: (Monad m) =>
       FileExtListenerT Response m a
    -> VerbListenerT Response m ()
get flistener = do
  (fileexts :: FileExts Response) <-
      lift $ execWriterT $ runFileExtListenerT flistener
  let new = foldrWithKey (\k -> insert (k, Get)) empty $ unFileExts fileexts

  VerbListenerT $ tell $ Verbs $ new

-- post :: MonadIO m =>
--         (ByteString -> m ())
--      -> FileExtListener ()
--      -> VerbListener ()
-- post fl =
--   VerbListener $ tell $
--     Verbs [(Post, fl)]
--
-- put :: MonadIO m =>
--        (ByteString -> m ())
--     -> FileExtListener ()
--     -> VerbListener ()
-- put fl =
--   VerbListener $ tell $
--     Verbs [(Put, fl)]

-- delete :: (Monad m) =>
--           FileExtListenerT m ()
--        -> VerbListenerT m ()
-- delete fl =
--   VerbListenerT $ tell $
--     Verbs [(Post, fl)]
