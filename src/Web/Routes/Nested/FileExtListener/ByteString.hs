{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.ByteString where

import           Web.Routes.Nested.FileExtListener.Types

import qualified Data.ByteString.Lazy                    as B
import           Data.Map
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai
import qualified Network.Wai.Util                        as U

import           Control.Monad.Writer


-- | @ByteString@ is ambiguous - we need to know what @RequestHeaders@ and @FileExt@ should be associated.
bytestring :: Monad m => FileExt -> RequestHeaders -> B.ByteString -> FileExtListenerT Response m ()
bytestring e = bytestringStatus e status200

bytestringWith :: Monad m => (Response -> Response) -> FileExt -> RequestHeaders -> B.ByteString -> FileExtListenerT Response m ()
bytestringWith f e = bytestringStatusWith f e status200

bytestringStatus :: Monad m => FileExt -> Status -> RequestHeaders -> B.ByteString
                 -> FileExtListenerT Response m ()
bytestringStatus = bytestringStatusWith id

bytestringStatusWith :: Monad m =>
                        (Response -> Response)
                     -> FileExt -> Status -> RequestHeaders -> B.ByteString
                     -> FileExtListenerT Response m ()
bytestringStatusWith f e s hs i = do
  r <- lift $ U.bytestring s hs i
  FileExtListenerT $ tell $
    FileExts $ singleton e $ f r


bytestringOnly :: RequestHeaders -> B.ByteString -> Response
bytestringOnly = bytestringOnlyStatus status200

-- | The exact same thing as @Network.Wai.responseLBS@.
bytestringOnlyStatus :: Status -> RequestHeaders -> B.ByteString -> Response
bytestringOnlyStatus = responseLBS
