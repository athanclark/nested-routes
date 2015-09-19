{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Lucid where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import qualified Lucid.Base                              as L
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | Uses the @Html@ key in the map, and @"text/html"@ as the content type.
lucid :: Monad m => L.HtmlT m () -> FileExtListenerT Response m ()
lucid = lucidStatusHeaders status200 [("Content-Type", "text/html")]

lucidWith :: Monad m => (Response -> Response) -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidWith f = lucidStatusHeadersWith f status200 [("Content-Type", "text/html")]

lucidStatus :: Monad m => Status -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidStatus s = lucidStatusHeaders s [("Content-Type", "text/html")]

lucidStatusWith :: Monad m => (Response -> Response) -> Status -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidStatusWith f s = lucidStatusHeadersWith f s [("Content-Type", "text/html")]

lucidHeaders :: Monad m => RequestHeaders -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidHeaders = lucidStatusHeaders status200

lucidHeadersWith :: Monad m => (Response -> Response) -> RequestHeaders -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidHeadersWith f = lucidStatusHeadersWith f status200

lucidStatusHeaders :: Monad m => Status -> RequestHeaders -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidStatusHeaders = lucidStatusHeadersWith id

lucidStatusHeadersWith :: Monad m => (Response -> Response) -> Status -> RequestHeaders -> L.HtmlT m () -> FileExtListenerT Response m ()
lucidStatusHeadersWith f s hs i = do
  r <- lift $ lucidOnlyStatusHeaders s hs i
  FileExtListenerT $ tell $
    FileExts $ singleton Html $ f r




lucidOnly :: Monad m =>
             L.HtmlT m () -> m Response
lucidOnly = lucidOnlyStatusHeaders status200 [("Content-Type", "text/html")]

lucidOnlyStatus :: Monad m =>
             Status -> L.HtmlT m () -> m Response
lucidOnlyStatus s = lucidOnlyStatusHeaders s [("Content-Type", "text/html")]

lucidOnlyHeaders :: Monad m =>
             RequestHeaders -> L.HtmlT m () -> m Response
lucidOnlyHeaders = lucidOnlyStatusHeaders status200

lucidOnlyStatusHeaders :: Monad m =>
             Status -> RequestHeaders -> L.HtmlT m () -> m Response
lucidOnlyStatusHeaders s hs i = liftM (bytestringOnlyStatus s hs) $ L.renderBST i
