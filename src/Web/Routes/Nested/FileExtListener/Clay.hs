{-# LANGUAGE OverloadedStrings #-}



module Web.Routes.Nested.FileExtListener.Clay where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import           Clay.Render
import           Clay.Stylesheet
import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | Uses @Text@ as the key in the map, and @"text/css"@ as the content type.
clay :: Monad m => Config -> [App] -> Css -> FileExtListenerT Response m ()
clay c as = clayStatusHeaders c as status200 [("Content-Type", "text/css")]

clayStatus :: Monad m =>Config -> [App] ->  Status -> Css -> FileExtListenerT Response m ()
clayStatus c as s = clayStatusHeaders c as s [("Content-Type", "text/css")]

clayHeaders :: Monad m => Config -> [App] -> RequestHeaders -> Css -> FileExtListenerT Response m ()
clayHeaders c as = clayStatusHeaders c as status200

clayStatusHeaders :: Monad m => Config -> [App] -> Status -> RequestHeaders -> Css -> FileExtListenerT Response m ()
clayStatusHeaders c as s hs i =
  let r = clayOnlyStatusHeaders c as s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton Css r




clayOnly :: Config -> [App] -> Css -> Response
clayOnly c as = clayOnlyStatusHeaders c as status200 [("Content-Type", "text/css")]

clayOnlyStatus :: Config -> [App] -> Status -> Css -> Response
clayOnlyStatus c as s = clayOnlyStatusHeaders c as s [("Content-Type", "text/css")]

clayOnlyHeaders :: Config -> [App] -> RequestHeaders -> Css -> Response
clayOnlyHeaders c as = clayOnlyStatusHeaders c as status200

clayOnlyStatusHeaders :: Config -> [App] -> Status -> RequestHeaders -> Css -> Response
clayOnlyStatusHeaders c as s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderWith c as i





