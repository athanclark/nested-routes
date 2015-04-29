{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.ByteString where

import           Web.Routes.Nested.FileExtListener.Types

import qualified Data.ByteString.Lazy                    as B
import           Data.Map
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | @ByteString@ is ambiguous - we need to know what @RequestHeaders@ and @FileExt@ should be associated.
bytestring :: Monad m => FileExt -> RequestHeaders -> B.ByteString -> FileExtListenerT Response m ()
bytestring e hs i =
  let r = responseLBS status200 hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r

bytestringStatus :: Monad m => FileExt -> Status -> RequestHeaders -> B.ByteString -> FileExtListenerT Response m ()
bytestringStatus e s hs i =
  let r = responseLBS s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r


bytestringOnly :: RequestHeaders -> B.ByteString -> Response
bytestringOnly = responseLBS status200

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnlyStatus :: Status -> RequestHeaders -> B.ByteString -> Response
bytestringOnlyStatus = responseLBS

