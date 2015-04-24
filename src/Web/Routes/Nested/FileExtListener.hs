{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Routes.Nested.FileExtListener where

import qualified Data.Aeson              as A
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy    as B
import qualified Data.ByteString.Builder as BU
import qualified Text.Blaze.Html         as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Lucid.Base              as L
import           Network.HTTP.Types      (status200, RequestHeaders)
import           Network.Wai

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Monoid
import           Data.Map.Lazy
import           Data.Traversable
import           Data.Foldable hiding (elem)


data FileExt = Html
             | Json
             | Text
  deriving (Show, Eq, Ord)


toExt :: T.Text -> Maybe FileExt
toExt x | x `elem` htmls = Just Html
        | x `elem` jsons = Just Json
        | x `elem` texts = Just Text
        | otherwise      = Nothing
  where
    htmls = ["", ".htm", ".html"]
    jsons = [".json"]
    texts = [".txt"]

newtype FileExts a = FileExts { unFileExts :: Map FileExt a }
  deriving (Show, Eq, Functor, Traversable)

deriving instance Monoid      (FileExts a)
deriving instance Foldable     FileExts


newtype FileExtListenerT r m a =
  FileExtListenerT { runFileExtListenerT :: WriterT (FileExts r) m a }
    deriving (Functor)

deriving instance Applicative m => Applicative (FileExtListenerT r m)
deriving instance Monad m =>       Monad       (FileExtListenerT r m)
deriving instance MonadIO m =>     MonadIO     (FileExtListenerT r m)
deriving instance                  MonadTrans  (FileExtListenerT r)


json :: (A.ToJSON j, Monad m) =>
        j -> FileExtListenerT Response m ()
json i =
  let r = responseLBS status200 [("Content-Type", "application/json")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonOnly :: (A.ToJSON j) =>
            j -> Response
jsonOnly i =
  responseLBS status200 [("Content-Type", "application/json")] $ A.encode i

jsonp :: (A.ToJSON j, Monad m) =>
         j -> FileExtListenerT Response m ()
jsonp i =
  let r = responseLBS status200 [("Content-Type", "application/javascript")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonpOnly :: (A.ToJSON j) =>
            j -> Response
jsonpOnly i =
  responseLBS status200 [("Content-Type", "application/javascript")] $ A.encode i

text :: (Monad m) =>
        LT.Text -> FileExtListenerT Response m ()
text i =
  let r = responseLBS status200 [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r

textOnly :: LT.Text -> Response
textOnly i =
  responseLBS status200 [("Content-Type", "text/plain")] $ LT.encodeUtf8 i

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

lucid :: (Monad m) =>
         L.HtmlT m () -> FileExtListenerT Response m ()
lucid i = do
  i' <- lift $ L.renderBST i
  let r = responseLBS status200 [("Content-Type", "text/html")] $ i'
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

lucidOnly :: (Monad m) =>
             L.HtmlT m () -> m Response
lucidOnly i = do
  i' <- L.renderBST i
  return $ responseLBS status200 [("Content-Type", "text/html")] $ i'

builder :: (Monad m) =>
           BU.Builder -> RequestHeaders
        -> FileExt -> FileExtListenerT Response m ()
builder i hs e =
  let r = responseBuilder status200 hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r

builderOnly :: BU.Builder -> RequestHeaders -> Response
builderOnly i hs =
  responseBuilder status200 hs i

bytestring :: (Monad m) =>
              B.ByteString -> RequestHeaders
           -> FileExt -> FileExtListenerT Response m ()
bytestring i hs e =
  let r = responseLBS status200 hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton e r

bytestringOnly :: B.ByteString -> RequestHeaders -> Response
bytestringOnly i hs =
  responseLBS status200 hs i
