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
import           Data.Monoid
import           Data.Foldable
import           Data.Traversable
import           Data.Map.Lazy
import qualified Data.ByteString                   as B


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

newtype Verbs a = Verbs { unVerbs :: Map Verb (FileExts a) }
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
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Get fileexts
  VerbListenerT $ tell $ Verbs $ new

-- post :: (Monad m, MonadIO m) =>
--         (ByteString -> m ())
--      -> FileExtListenerT Response m a
--      -> VerbListenerT Response m ()
-- post handle fl = do
--   rbody <- getUntilM (== B.empty) $ requestBody
--   VerbListener $ tell $
--     Verbs [(Post, fl)]
--
--   where
--    getUntilM p ma = do
--      a <- ma
--      if p a then return a
--             else do b <- getUntilM p ma
--                     a <> b

-- put :: (Monad m, MonadIO m) =>
--        (ByteString -> m ())
--     -> FileExtListener ()
--     -> VerbListener ()
-- put fl =
--   VerbListener $ tell $
--     Verbs [(Put, fl)]

delete :: (Monad m) =>
          FileExtListenerT Response m a
       -> VerbListenerT Response m ()
delete flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Delete fileexts
  VerbListenerT $ tell $ Verbs $ new
