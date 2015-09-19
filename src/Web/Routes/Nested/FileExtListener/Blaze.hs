{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Blaze where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai
import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Text           as H

import           Control.Monad.Writer


-- | Uses @Html@ as the key in the map, and @"text/html"@ as the content type.
blaze :: Monad m => H.Html -> FileExtListenerT Response m ()
blaze = blazeStatusHeaders status200 [("Content-Type", "text/html")]

blazeWith :: Monad m => (Response -> Response) -> H.Html -> FileExtListenerT Response m ()
blazeWith f = blazeStatusHeadersWith f status200 [("Content-Type", "text/html")]

blazeStatus :: Monad m => Status -> H.Html -> FileExtListenerT Response m ()
blazeStatus s = blazeStatusHeaders s [("Content-Type", "text/html")]

blazeStatusWith :: Monad m => (Response -> Response) -> Status -> H.Html -> FileExtListenerT Response m ()
blazeStatusWith f s = blazeStatusHeadersWith f s [("Content-Type", "text/html")]

blazeHeaders :: Monad m => RequestHeaders -> H.Html -> FileExtListenerT Response m ()
blazeHeaders = blazeStatusHeaders status200

blazeHeadersWith :: Monad m => (Response -> Response) -> RequestHeaders -> H.Html -> FileExtListenerT Response m ()
blazeHeadersWith f = blazeStatusHeadersWith f status200

blazeStatusHeaders :: Monad m => Status -> RequestHeaders -> H.Html -> FileExtListenerT Response m ()
blazeStatusHeaders = blazeStatusHeadersWith id

blazeStatusHeadersWith :: Monad m => (Response -> Response) -> Status -> RequestHeaders -> H.Html -> FileExtListenerT Response m ()
blazeStatusHeadersWith f s hs i =
  let r = blazeOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html $ f r



blazeOnly :: H.Html -> Response
blazeOnly = blazeOnlyStatusHeaders status200 [("Content-Type", "text/html")]

blazeOnlyHeaders :: RequestHeaders -> H.Html -> Response
blazeOnlyHeaders = blazeOnlyStatusHeaders status200

blazeOnlyStatus :: Status -> H.Html -> Response
blazeOnlyStatus s = blazeOnlyStatusHeaders s [("Content-Type", "text/html")]

blazeOnlyStatusHeaders :: Status -> RequestHeaders -> H.Html -> Response
blazeOnlyStatusHeaders s hs i =
  bytestringOnlyStatus s hs $ LT.encodeUtf8 $ H.renderHtml i
