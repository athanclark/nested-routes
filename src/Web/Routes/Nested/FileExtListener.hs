{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Routes.Nested.FileExtListener where

import qualified Data.Aeson              as A
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Network.HTTP.Types      (status200)
import           Network.Wai

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Monoid


data FileExt = Html
             | Json
             | Text
  deriving (Show, Eq, Ord)

newtype FileExts a = FileExts { unFileExts :: [(FileExt, a)] }
  deriving (Functor)

-- | Sorting instance
instance Monoid (FileExts a) where
  mempty = FileExts []
  (FileExts []) `mappend` ys = ys
  xs `mappend` (FileExts []) = xs
  (FileExts (x'@(x,a):xs)) `mappend` (FileExts (y'@(y,b):ys))
    | x < y = FileExts $ x' : xs `mappend` (y':ys)
    | x > y = FileExts $ y' : (x':xs) `mappend` ys
    | x == y = FileExts $ y' : xs `mappend` ys
    | otherwise = error "unordered merge?"

newtype FileExtListenerT m a = FileExtListenerT
  { runFileExtListenerT :: WriterT (FileExts Response) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (FileExtListenerT m)
deriving instance Monad m =>       Monad       (FileExtListenerT m)
deriving instance MonadIO m =>     MonadIO     (FileExtListenerT m)
deriving instance                  MonadTrans   FileExtListenerT

-- html :: VerbListener () -> FileExtListener ()
-- html vl = FileExtListener $ tell $
--     FileExts [(Html, vl)]

json :: (A.ToJSON j, Monad m) =>
        j
     -> FileExtListenerT m ()
json i =
  let r = responseLBS status200 [("Content-Type", "application/json")] $
            A.encode i
  in
  FileExtListenerT $ tell $
    FileExts [(Json, r)]

jsonp :: (A.ToJSON j, Monad m) =>
         j
      -> FileExtListenerT m ()
jsonp i =
  let r = responseLBS status200 [("Content-Type", "application/javascript")] $
            A.encode i
  in
  FileExtListenerT $ tell $
    FileExts [(Json, r)]

text :: (Monad m) =>
        LT.Text
     -> FileExtListenerT m ()
text i =
  let r = responseLBS status200 [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i
  in
  FileExtListenerT $ tell $
    FileExts [(Text, r)]
--
-- blaze :: Html -> Response
-- blaze = HR.renderHtml
--
-- lucid :: Monad m => HtmlT m () -> Response
-- lucid = L.renderTextT

-- json :: ToJSON a => a -> Response
-- json = lazy-bytestring . encode

-- text :: T.Text -> Response
-- text = bytestring . TR.encodeUtf8

-- lazy-text :: LT.Text -> Response
-- lazy-text = lazy-bytestring . LTR.encodeUtf8
--
-- builder :: Builder -> Response
-- builder = responseBuilder
--
-- bytestring :: B.ByteString -> Response
-- bytestring = lazy-bytestring . LB.fromStrict
--
-- lazy-bytestring :: LB.ByteString -> Response
-- lazy-bytestring = responseLBS
