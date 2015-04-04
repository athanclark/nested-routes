{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Routes.Nested where

import           Web.Routes.Nested.VerbListener
import           Web.Routes.Nested.FileExtListener

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.AddHeaders

import           Control.Applicative
import           Control.Arrow                     (second)
import           Control.Monad.Writer
import qualified Data.List.NonEmpty                as NE
import           Data.Monoid
import           Data.Trie.Pseudo
import           Data.Trie.Rooted

newtype Handler a = Handler
  { runHandler :: Writer (MergeRooted String [(Verb, [(FileExt, Response)])]) a }
  deriving (Functor)

deriving instance Applicative Handler
deriving instance Monad Handler

handle :: [String] -> VerbListener () -> Handler ()
handle ts vl =
  let
    xs :: [(Verb, [(FileExt, Response)])]
    xs = fmap (second $ unFileExts . snd . runWriter . runFileExtListener) $
           unVerbs $ snd $ runWriter $ runVerbListener vl
  in
  Handler $ tell $
    case ts of
      [] -> MergeRooted $ Rooted (Just xs) []
      _  -> MergeRooted $ Rooted Nothing [Rest (NE.fromList ts) xs]


-- route :: Hanlder () -> Application
