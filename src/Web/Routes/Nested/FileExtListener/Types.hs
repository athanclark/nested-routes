{-# LANGUAGE
    DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}


module Web.Routes.Nested.FileExtListener.Types where

import qualified Data.Text            as T

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Foldable        hiding (elem)
import           Data.Map
import           Data.Monoid
import           Data.Traversable


-- | Supported file extensions
data FileExt = Html
             | Css
             | JavaScript
             | Json
             | Text
  deriving (Show, Eq, Ord)


toExt :: T.Text -> Maybe FileExt
toExt x | x `elem` htmls       = Just Html
        | x `elem` csss        = Just Css
        | x `elem` javascripts = Just JavaScript
        | x `elem` jsons       = Just Json
        | x `elem` texts       = Just Text
        | otherwise            = Nothing
  where
    htmls       = [".htm", ".html"]
    csss        = [".css"]
    javascripts = [".js", ".javascript"]
    jsons       = [".json"]
    texts       = [".txt"]

newtype FileExts a = FileExts { unFileExts :: Map FileExt a }
  deriving (Show, Eq, Monoid, Functor, Foldable, Traversable)

newtype FileExtListenerT r m a =
  FileExtListenerT { runFileExtListenerT :: WriterT (FileExts r) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

execFileExtListenerT :: Monad m => FileExtListenerT r m a -> m (FileExts r)
execFileExtListenerT = execWriterT . runFileExtListenerT
