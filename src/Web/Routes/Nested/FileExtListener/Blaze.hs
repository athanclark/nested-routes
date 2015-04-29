{-# LANGUAGE
    OverloadedStrings
  #-}


module Web.Routes.Nested.FileExtListener.Blaze where

import Web.Routes.Nested.FileExtListener.Types

import qualified Text.Blaze.Html         as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Data.Text.Lazy.Encoding as LT
import Data.Map
import           Network.HTTP.Types      (status200, RequestHeaders, Status)
import           Network.Wai

import           Control.Monad.Writer


blaze :: (Monad m ) =>
         H.Html -> FileExtListenerT Response m ()
blaze i =
  let r = responseLBS status200 [("Content-Type", "text/html")] $
            LT.encodeUtf8 $ H.renderHtml i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

blazeOnly :: H.Html -> Response
blazeOnly i =
  responseLBS status200 [("Content-Type", "text/html")] $ LT.encodeUtf8 $ H.renderHtml i
