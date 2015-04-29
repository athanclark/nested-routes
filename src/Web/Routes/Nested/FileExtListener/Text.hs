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



text :: Monad m => LT.Text -> FileExtListenerT Response m ()
text i =
  let r = responseLBS status200 [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r

textStatus :: Monad m => Status -> LT.Text -> FileExtListenerT Response m ()
textStatus s i =
  let r = responseLBS s [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r

textHeaders :: Monad m => RequestHeaders -> LT.Text -> FileExtListenerT Response m ()
textHeaders hs i =
  let r = responseLBS status200 hs $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r

textStatusHeaders :: Monad m => Status -> RequestHeaders -> LT.Text -> FileExtListenerT Response m ()
textStatusHeaders s hs i =
  let r = responseLBS s hs $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r




textOnly :: LT.Text -> Response
textOnly i =
  responseLBS status200 [("Content-Type", "text/plain")] $ LT.encodeUtf8 i

textOnlyStatus :: Status -> LT.Text -> Response
textOnlyStatus s i =
  responseLBS s [("Content-Type", "text/plain")] $ LT.encodeUtf8 i

textOnlyHeaders :: RequestHeaders -> LT.Text -> Response
textOnlyHeaders hs i =
  responseLBS status200 hs $ LT.encodeUtf8 i

textOnlyStatusHeaders :: Status -> RequestHeaders -> LT.Text -> Response
textOnlyStatusHeaders s hs i =
  responseLBS s hs $ LT.encodeUtf8 i





