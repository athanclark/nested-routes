{-# LANGUAGE
    OverloadedStrings
  #-}


module Web.Routes.Nested.FileExtListener.Lucid where

import Web.Routes.Nested.FileExtListener.Types

import qualified Lucid.Base              as L
import Data.Map
import           Network.HTTP.Types      (status200, RequestHeaders, Status)
import           Network.Wai

import           Control.Monad.Writer


lucid :: (Monad m) =>
         L.HtmlT m () -> FileExtListenerT Response m ()
lucid i = do
  i' <- lift $ L.renderBST i
  let r = responseLBS status200 [("Content-Type", "text/html")] i'
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

lucidOnly :: (Monad m) =>
             L.HtmlT m () -> m Response
lucidOnly i = do
  i' <- L.renderBST i
  return $ responseLBS status200 [("Content-Type", "text/html")] i'
