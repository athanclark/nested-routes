{-# LANGUAGE
    OverloadedStrings
  #-}


module Web.Routes.Nested.FileExtListener.ByteString where

import Web.Routes.Nested.FileExtListener.Types

import qualified Data.ByteString.Lazy    as B
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Map
import           Network.HTTP.Types      (status200, RequestHeaders, Status)
import           Network.Wai

import           Control.Monad.Trans
import           Control.Monad.Writer


bytestring :: (Monad m) =>
              B.ByteString -> RequestHeaders
           -> FileExt -> FileExtListenerT Response m ()
bytestring i hs e =
  let r = responseLBS status200 hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r


bytestringOnly :: B.ByteString -> RequestHeaders -> Response
bytestringOnly i hs =
  responseLBS status200 hs i


bytestringStatus :: (Monad m) =>
                    B.ByteString -> Status -> RequestHeaders
                 -> FileExt -> FileExtListenerT Response m ()
bytestringStatus i s hs e =
  let r = responseLBS s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r
