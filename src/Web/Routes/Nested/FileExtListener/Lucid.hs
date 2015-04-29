{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Lucid where

import           Web.Routes.Nested.FileExtListener.Types

import           Data.Map
import qualified Lucid.Base                              as L
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer



lucid :: Monad m =>
         L.HtmlT m () -> FileExtListenerT Response m ()
lucid i = do
  i' <- lift $ L.renderBST i
  let r = responseLBS status200 [("Content-Type", "text/html")] i'
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

lucidStatus :: Monad m =>
         Status -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidStatus s i = do
  i' <- lift $ L.renderBST i
  let r = responseLBS s [("Content-Type", "text/html")] i'
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

lucidHeaders :: Monad m =>
         RequestHeaders -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidHeaders hs i = do
  i' <- lift $ L.renderBST i
  let r = responseLBS status200 hs i'
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

lucidStatusHeaders :: Monad m =>
         Status -> RequestHeaders -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidStatusHeaders s hs i = do
  i' <- lift $ L.renderBST i
  let r = responseLBS s hs i'
  FileExtListenerT $ tell $
    FileExts $ singleton Html r




lucidOnly :: Monad m =>
             L.HtmlT m () -> m Response
lucidOnly i = do
  i' <- L.renderBST i
  return $ responseLBS status200 [("Content-Type", "text/html")] i'

lucidOnlyStatus :: Monad m =>
             Status -> L.HtmlT m () -> m Response
lucidOnlyStatus s i = do
  i' <- L.renderBST i
  return $ responseLBS s [("Content-Type", "text/html")] i'

lucidOnlyHeaders :: Monad m =>
             RequestHeaders -> L.HtmlT m () -> m Response
lucidOnlyHeaders hs i = do
  i' <- L.renderBST i
  return $ responseLBS status200 hs i'

lucidOnlyStatusHeaders :: Monad m =>
             Status -> RequestHeaders -> L.HtmlT m () -> m Response
lucidOnlyStatusHeaders s hs i = do
  i' <- L.renderBST i
  return $ responseLBS s hs i'







