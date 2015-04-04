{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Web.Routes.Nested.FileExtListener where

import           Network.Wai
import Network.HTTP.Types (status200)
import qualified Data.Aeson           as A
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import           Control.Applicative
import           Control.Monad.Writer
import           Data.Monoid


data FileExt = Html
             | Json
             | Text
  deriving (Show, Eq, Ord)

newtype FileExts a = FileExts { unFileExts :: [(FileExt, a)] }
  deriving (Functor)

instance Monoid (FileExts a) where
  mempty = FileExts []
  (FileExts []) `mappend` ys = ys
  xs `mappend` (FileExts []) = xs
  (FileExts (x'@(x,a):xs)) `mappend` (FileExts (y'@(y,b):ys))
    | x < y = FileExts $ x' : xs `mappend` (y':ys)
    | x > y = FileExts $ y' : (x':xs) `mappend` ys
    | x == y = FileExts $ y' : xs `mappend` ys
    | otherwise = error "unordered merge?"

newtype FileExtListener a = FileExtListener
  { runFileExtListener :: Writer (FileExts Response) a }
  deriving (Functor)

deriving instance Applicative FileExtListener
deriving instance Monad FileExtListener

-- html :: VerbListener () -> FileExtListener ()
-- html vl = FileExtListener $ tell $
--     FileExts [(Html, vl)]

json :: A.ToJSON j => j -> FileExtListener ()
json i =
  let r = responseLBS status200 [("Content-Type", "application/json")] $
            A.encode i
  in
  FileExtListener $ tell $
    FileExts [(Json, r)]

text :: LT.Text -> FileExtListener ()
text i =
  let r = responseLBS status200 [("Content-Type", "text/plain")] $
            LT.encodeUtf8 i
  in
  FileExtListener $ tell $
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
