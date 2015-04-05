{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Routes.Nested
  ( module Web.Routes.Nested.FileExtListener
  , module Web.Routes.Nested.VerbListener
  , handle
  , route
  ) where

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
import qualified Data.Trie.Rooted                  as R
import           Data.Traversable
import qualified Data.Text                         as T
import qualified Data.Map.Lazy                     as M
import Debug.Trace (traceShow)


newtype HandlerT m a = HandlerT
  { runHandler :: WriterT (MergeRooted T.Text (Verbs Response)) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT m)
deriving instance Monad m =>       Monad       (HandlerT m)
deriving instance MonadIO m =>     MonadIO     (HandlerT m)
deriving instance                  MonadTrans   HandlerT


handle :: Monad m =>
          [T.Text]
       -> VerbListenerT Response m ()
       -> HandlerT m ()
handle ts vl = do
  vfrs <- lift $ execWriterT $ runVerbListenerT vl

  HandlerT $ tell $
    case ts of
      [] -> MergeRooted $ Rooted (Just vfrs) []
      _  -> MergeRooted $ Rooted Nothing [Rest (NE.fromList ts) vfrs]

route :: (Functor m, Monad m) =>
         HandlerT m a
      -> Request -> (Response -> m b) -> m b
route h req respond = do
  trie <- unMergeRooted <$> (execWriterT $ runHandler h)
  let mMethod = httpMethodToMSym $ requestMethod req
      mFileext = case pathInfo req of
                   [] -> Just Html
                   xs -> possibleExts $ T.pack $ getFileExt $
                                        T.unpack $ last xs

  case (mFileext, mMethod) of
    (Just f, Just v) -> case R.lookup (pathInfo req) trie of
      Just vmap -> case M.lookup v $ unVerbs vmap of
        Just fmap -> case M.lookupGE f $ unFileExts fmap of
          Just (_,r) -> respond r
          Nothing -> respond notFound
        Nothing -> respond notFound
      Nothing  -> respond notFound
    _ -> respond notFound

  where
    getFileExt :: String -> String
    getFileExt s = case foldr go Nothing s of
                     Nothing -> ""
                     Just x  -> x
      where
        go '.' _         = Just "."
        go x   (Just xs) = Just $ xs ++ [x]
        go x   Nothing   = Nothing

    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just Get
                       | x == methodPost   = Just Post
                       | x == methodPut    = Just Put
                       | x == methodDelete = Just Delete
                       | otherwise         = Nothing

    notFound = responseLBS status404 [("Content-Type","text/plain")] "404 :("
