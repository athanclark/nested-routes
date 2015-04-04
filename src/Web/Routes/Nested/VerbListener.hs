{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Routes.Nested.VerbListener where

import Web.Routes.Nested.FileExtListener

import Data.Monoid
import Network.Wai

import Control.Applicative
import Control.Monad.Writer


data Verb = Get
          | Post
          | Put
          | Delete
  deriving (Show, Eq, Ord)

newtype Verbs a = Verbs { unVerbs :: [(Verb, a)] }
  deriving (Show, Eq, Functor)

instance Monoid (Verbs a) where
  mempty = Verbs []
  (Verbs []) `mappend` ys = ys
  xs `mappend` (Verbs []) = xs
  (Verbs (x'@(x,a):xs)) `mappend` (Verbs (y'@(y,b):ys))
    | x < y = Verbs $ x' : xs `mappend` (y':ys)
    | x > y = Verbs $ y' : (x':xs) `mappend` ys
    | x == y = Verbs $ y' : xs `mappend` ys
    | otherwise = error "unordered merge?"

-- | TODO: Change mappend to sorted / ordered merge
newtype VerbListener a = VerbListener
  { runVerbListener :: Writer (Verbs (FileExtListener ())) a }
  deriving (Functor)

deriving instance Applicative VerbListener
deriving instance Monad VerbListener

get :: FileExtListener () -> VerbListener ()
get fl =
  VerbListener $ tell $
    Verbs [(Get, fl)]

post :: FileExtListener () -> VerbListener ()
post fl =
  VerbListener $ tell $
    Verbs [(Post, fl)]

put :: FileExtListener () -> VerbListener ()
put fl =
  VerbListener $ tell $
    Verbs [(Put, fl)]

delete :: FileExtListener () -> VerbListener ()
delete fl =
  VerbListener $ tell $
    Verbs [(Post, fl)]
