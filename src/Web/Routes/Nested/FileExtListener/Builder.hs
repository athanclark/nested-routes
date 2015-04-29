{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Builder where

import           Web.Routes.Nested.FileExtListener.Types

import qualified Data.ByteString.Builder                 as BU
import           Data.Map
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | A builder is ambiguous, therefore we require @RequestHeaders@ and a @FileExt@ to be explicitly
-- supplied.
builder :: Monad m => FileExt -> RequestHeaders -> BU.Builder -> FileExtListenerT Response m ()
builder e hs i =
  let r = responseBuilder status200 hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r

builderStatus :: Monad m => FileExt -> Status -> RequestHeaders -> BU.Builder -> FileExtListenerT Response m ()
builderStatus e s hs i =
  let r = responseBuilder s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r



builderOnly :: RequestHeaders -> BU.Builder -> Response
builderOnly = responseBuilder status200

-- | The exact same thing as @Network.Wai.responseBuilder@.
builderOnlyStatus :: Status -> RequestHeaders -> BU.Builder -> Response
builderOnlyStatus = responseBuilder
