{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Routes.Nested where

import           Web.Routes.Nested.FileExtListener
import           Web.Routes.Nested.VerbListener

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.AddHeaders

import           Control.Applicative
import           Control.Arrow                     (second, first, (***))
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer
import qualified Data.List.NonEmpty                as NE
import           Data.Monoid
import           Data.Trie.Pseudo
import           Data.Trie.Rooted
import           Data.Traversable

newtype HandlerT m a = HandlerT
  { runHandler :: WriterT (MergeRooted String (Verbs (FileExts Response))) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT m)
deriving instance Monad m =>       Monad       (HandlerT m)
deriving instance MonadIO m =>     MonadIO     (HandlerT m)
deriving instance                  MonadTrans   HandlerT


handle :: Monad m =>
          [String]
       -> VerbListenerT (FileExts Response) m ()
       -> HandlerT m ()
handle ts vl = do
  vfrs <- lift $ execWriterT $ runVerbListenerT vl

  HandlerT $ tell $
    case ts of
      [] -> MergeRooted $ Rooted (Just vfrs) []
      _  -> MergeRooted $ Rooted Nothing [Rest (NE.fromList ts) vfrs]

route :: (Monad m, Functor m) => HandlerT m () -> m Application
route h = do
  trie <- unMergeRooted <$> (execWriterT $ runHandler h)
  undefined
