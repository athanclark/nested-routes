{-# LANGUAGE OverloadedStrings #-}



module Web.Routes.Nested.FileExtListener.Lucius where

import           Web.Routes.Nested.FileExtListener.Types as FE
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import           Text.Lucius
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | Uses @lucius@ as the key in the map, and @"lucius/css"@ as the content type.
lucius :: Monad m => Css -> FileExtListenerT Response m ()
lucius = luciusStatusHeaders status200 [("Content-Type", "lucius/css")]

luciusWith :: Monad m => (Response -> Response) -> Css -> FileExtListenerT Response m ()
luciusWith f = luciusStatusHeadersWith f status200 [("Content-Type", "lucius/css")]

luciusStatus :: Monad m => Status -> Css -> FileExtListenerT Response m ()
luciusStatus s = luciusStatusHeaders s [("Content-Type", "lucius/css")]

luciusStatusWith :: Monad m => (Response -> Response) -> Status -> Css -> FileExtListenerT Response m ()
luciusStatusWith f s = luciusStatusHeadersWith f s [("Content-Type", "lucius/css")]

luciusHeaders :: Monad m => RequestHeaders -> Css -> FileExtListenerT Response m ()
luciusHeaders = luciusStatusHeaders status200

luciusHeadersWith :: Monad m => (Response -> Response) -> RequestHeaders -> Css -> FileExtListenerT Response m ()
luciusHeadersWith f = luciusStatusHeadersWith f status200

luciusStatusHeaders :: Monad m => Status -> RequestHeaders -> Css -> FileExtListenerT Response m ()
luciusStatusHeaders = luciusStatusHeadersWith id

luciusStatusHeadersWith :: Monad m => (Response -> Response) -> Status -> RequestHeaders -> Css -> FileExtListenerT Response m ()
luciusStatusHeadersWith f s hs i =
  let r = luciusOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton FE.Css $ f r




luciusOnly :: Css -> Response
luciusOnly = luciusOnlyStatusHeaders status200 [("Content-Type", "lucius/css")]

luciusOnlyStatus :: Status -> Css -> Response
luciusOnlyStatus s = luciusOnlyStatusHeaders s [("Content-Type", "lucius/css")]

luciusOnlyHeaders :: RequestHeaders -> Css -> Response
luciusOnlyHeaders = luciusOnlyStatusHeaders status200

luciusOnlyStatusHeaders :: Status -> RequestHeaders -> Css -> Response
luciusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderCss i
