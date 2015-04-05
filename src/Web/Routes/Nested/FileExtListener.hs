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
import qualified Text.Blaze.Html         as H
import qualified Text.Blaze.Html.Renderer.Text as H
import           Network.HTTP.Types      (status200)
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


possibleExts :: T.Text -> Maybe FileExt
possibleExts x | x `elem` htmls = Just Html
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


jsonp :: (A.ToJSON j, Monad m) =>
         j -> FileExtListenerT Response m ()
jsonp i =
  let r = responseLBS status200 [("Content-Type", "application/javascript")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r


text :: (Monad m) =>
        LT.Text -> FileExtListenerT Response m ()
text i =
  let r = responseLBS status200 [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i in
  FileExtListenerT $ tell $
    FileExts $ singleton Text r


blaze :: (Monad m ) =>
         H.Html -> FileExtListenerT Response m ()
blaze i =
  let r = responseLBS status200 [("Content-Type", "text/html")] $
            LT.encodeUtf8 $ H.renderHtml i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

-- lucid :: Monad m => HtmlT m () -> Response
-- lucid = L.renderTextT

-- builder :: Builder -> Response
-- builder = responseBuilder
--
-- bytestring :: B.ByteString -> Response
-- bytestring = lazy-bytestring . LB.fromStrict
--
-- lazy-bytestring :: LB.ByteString -> Response
-- lazy-bytestring = responseLBS
