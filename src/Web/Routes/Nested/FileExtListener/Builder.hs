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
builder e  = builderStatus e status200

builderWith :: Monad m => (Response -> Response) -> FileExt -> RequestHeaders -> BU.Builder -> FileExtListenerT Response m ()
builderWith f e = builderStatusWith f e status200

builderStatus :: Monad m => FileExt -> Status -> RequestHeaders -> BU.Builder -> FileExtListenerT Response m ()
builderStatus = builderStatusWith id

builderStatusWith :: Monad m => (Response -> Response) -> FileExt -> Status -> RequestHeaders -> BU.Builder -> FileExtListenerT Response m ()
builderStatusWith f e s hs i =
  let r = builderOnlyStatus s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e $ f r



builderOnly :: RequestHeaders -> BU.Builder -> Response
builderOnly = builderOnlyStatus status200

-- | The exact same thing as @Network.Wai.responseBuilder@.
builderOnlyStatus :: Status -> RequestHeaders -> BU.Builder -> Response
builderOnlyStatus = responseBuilder
