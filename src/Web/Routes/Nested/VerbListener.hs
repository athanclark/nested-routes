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
import qualified Data.ByteString.Lazy                 as BL


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

type ReqBodyT z = BL.ByteString -> z

newtype Verbs z a = Verbs { unVerbs :: Map Verb (Maybe (ReqBodyT z), FileExts a) }
  deriving (Functor, Traversable)

deriving instance             Monoid    (Verbs z a)
deriving instance             Foldable  (Verbs z)

newtype VerbListenerT z r m a =
  VerbListenerT { runVerbListenerT :: WriterT (Verbs z r) m a }
    deriving (Functor)

deriving instance Applicative m => Applicative (VerbListenerT z r m)
deriving instance Monad m =>       Monad       (VerbListenerT z r m)
deriving instance MonadIO m =>     MonadIO     (VerbListenerT z r m)
deriving instance                  MonadTrans  (VerbListenerT z r)


get :: (Monad m) =>
       FileExtListenerT Response m a
    -> VerbListenerT z Response m ()
get flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Get (Nothing, fileexts)
  VerbListenerT $ tell $ Verbs new

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
       -> VerbListenerT z Response m ()
delete flistener = do
  (fileexts :: FileExts Response) <- lift $ execWriterT $
                                     runFileExtListenerT flistener
  let new = singleton Delete (Nothing, fileexts)
  VerbListenerT $ tell $ Verbs new
