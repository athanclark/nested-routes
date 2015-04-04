{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Routes.Nested.VerbListener where

import           Web.Routes.Nested.FileExtListener

import           Network.Wai

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Monoid


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

newtype Verbs a = Verbs { unVerbs :: [(Verb, a)] }
  deriving (Show, Eq, Functor)

-- | Sorting instance
instance Monoid (Verbs a) where
  mempty = Verbs []
  (Verbs []) `mappend` ys = ys
  xs `mappend` (Verbs []) = xs
  (Verbs (x'@(x,a):xs)) `mappend` (Verbs (y'@(y,b):ys))
    | x < y = Verbs $ x' : xs `mappend` (y':ys)
    | x > y = Verbs $ y' : (x':xs) `mappend` ys
    | x == y = Verbs $ y' : xs `mappend` ys
    | otherwise = error "unordered merge?"

newtype VerbListenerT m a = VerbListenerT
  { runVerbListenerT :: WriterT (Verbs (FileExts Response)) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (VerbListenerT m)
deriving instance Monad m =>       Monad       (VerbListenerT m)
deriving instance MonadIO m =>     MonadIO     (VerbListenerT m)
deriving instance                  MonadTrans   VerbListenerT


get :: (Monad m) =>
       FileExtListenerT m ()
    -> VerbListenerT m ()
get flistener = do
  (fileexts :: FileExts Response) <- lift $
            execWriterT $ runFileExtListenerT flistener
  VerbListenerT $ tell $
    Verbs [(Get, fileexts)]

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
