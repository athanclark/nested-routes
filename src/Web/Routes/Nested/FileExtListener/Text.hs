{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Text where

import           Web.Routes.Nested.FileExtListener.Types

import           Data.Map
import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


text :: (Monad m) =>
        LT.Text -> FileExtListenerT Response m ()
text i =
  let r = responseLBS status200 [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r

textOnly :: LT.Text -> Response
textOnly i =
  responseLBS status200 [("Content-Type", "text/plain")] $ LT.encodeUtf8 i
