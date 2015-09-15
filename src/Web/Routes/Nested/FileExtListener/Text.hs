{-# LANGUAGE OverloadedStrings #-}



module Web.Routes.Nested.FileExtListener.Text where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Data.Composition
import           Control.Monad.Writer


-- | Uses @Text@ as the key in the map, and @"text/plain"@ as the content type.
text :: Monad m => LT.Text -> FileExtListenerT Response m ()
text = textStatusHeaders status200 [("Content-Type", "text/plain")]

textStatus :: Monad m => Status -> LT.Text -> FileExtListenerT Response m ()
textStatus s = textStatusHeaders s [("Content-Type", "text/plain")]

textHeaders :: Monad m => RequestHeaders -> LT.Text -> FileExtListenerT Response m ()
textHeaders = textStatusHeaders status200

textStatusHeaders :: Monad m => Status -> RequestHeaders -> LT.Text -> FileExtListenerT Response m ()
textStatusHeaders = textStatusHeadersWith id

textStatusHeadersWith :: Monad m =>
                         (Response -> Response) -> Status -> RequestHeaders -> LT.Text
                      -> FileExtListenerT Response m ()
textStatusHeadersWith f s hs i =
  let r = textOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text $ f r




textOnly :: LT.Text -> Response
textOnly = textOnlyStatusHeaders status200 [("Content-Type", "text/plain")]

textOnlyStatus :: Status -> LT.Text -> Response
textOnlyStatus s = textOnlyStatusHeaders s [("Content-Type", "text/plain")]

textOnlyHeaders :: RequestHeaders -> LT.Text -> Response
textOnlyHeaders = textOnlyStatusHeaders status200

textOnlyStatusHeaders :: Status -> RequestHeaders -> LT.Text -> Response
textOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 i
