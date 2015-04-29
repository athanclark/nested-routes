{-# LANGUAGE
    OverloadedStrings
  #-}


module Web.Routes.Nested.FileExtListener.Builder where

import Web.Routes.Nested.FileExtListener.Types

import qualified Data.ByteString.Builder as BU
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Map
import           Network.HTTP.Types      (status200, RequestHeaders, Status)
import           Network.Wai

import           Control.Monad.Trans
import           Control.Monad.Writer


builder :: (Monad m) =>
           BU.Builder -> RequestHeaders
        -> FileExt -> FileExtListenerT Response m ()
builder i hs e =
  let r = responseBuilder status200 hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r

builderOnly :: BU.Builder -> RequestHeaders -> Response
builderOnly i hs =
  responseBuilder status200 hs i
