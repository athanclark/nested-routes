{-# LANGUAGE
    GADTs
  , TypeOperators
  , TypeFamilies
  , KindSignatures
  , DataKinds
  , RankNTypes
  , FlexibleInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  , ConstraintKinds
  #-}

module Web.Routes.Nested.Types
  ( Singleton (..)
  , Extend (..)
  , Extrude (..)
  , CatMaybes
  , module Web.Routes.Nested.Types.UrlChunks
  ) where

import           Data.Attoparsec.Text
import           Text.Regex
import           Web.Routes.Nested.Types.UrlChunks
import qualified Data.Text as T
import           Data.Trie.Pred
import           Data.Trie.Pred.Step
import qualified Data.Trie.Map as MT
import qualified Data.Map as Map


type family CatMaybes (xs :: [Maybe *]) :: [*] where
  CatMaybes '[] = '[]
  CatMaybes ('Nothing  ': xs) = CatMaybes xs
  CatMaybes (('Just x) ': xs) = x ': CatMaybes xs

-- | Creates a string of nodes - a trie with a width of 1.
class Singleton chunks a trie | chunks a -> trie where
  singleton :: chunks -> a -> trie

-- Basis
instance Singleton (UrlChunks '[]) a (RootedPredTrie T.Text a) where
  singleton Root r' = RootedPredTrie (Just r') emptyPT

-- Successor
instance ( Singleton (UrlChunks xs) a trie0
         , Extend (EitherUrlChunk x) trie0 trie1
         ) => Singleton (UrlChunks (x ': xs)) a trie1 where
  singleton (Cons u us) r' = extend u (singleton us r')


-- | Turn a list of tries (@Rooted@) into a node with those children
class Extend eitherUrlChunk child result | eitherUrlChunk child -> result where
  extend :: eitherUrlChunk -> child -> result

-- | Literal case
instance Extend (EitherUrlChunk  'Nothing) (RootedPredTrie T.Text a) (RootedPredTrie T.Text a) where
  extend ((:=) t) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie (MT.MapStep $ Map.singleton t (mx, Just xs)) mempty

-- | Existentially quantified case
instance Extend (EitherUrlChunk ('Just r)) (RootedPredTrie T.Text (r -> a)) (RootedPredTrie T.Text a) where
  extend ((:~) (i,q)) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie mempty $ PredSteps [PredStep i (eitherToMaybe . parseOnly q) mx xs]
  extend ((:*) (i,q)) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie mempty $ PredSteps [PredStep i (matchRegex q . T.unpack) mx xs]


-- | @FoldR Extend start chunks ~ result@
class Extrude chunks start result | chunks start -> result where
  extrude :: chunks -> start -> result

-- Basis
instance Extrude (UrlChunks '[]) (RootedPredTrie T.Text a) (RootedPredTrie T.Text a) where
  extrude Root r' = r'

-- Successor
instance ( Extrude (UrlChunks xs) trie0 trie1
         , Extend (EitherUrlChunk x) trie1 trie2 ) => Extrude (UrlChunks (x ': xs)) trie0 trie2 where
  extrude (Cons u us) r' = extend u (extrude us r')


eitherToMaybe :: Either String r -> Maybe r
eitherToMaybe (Right r') = Just r'
eitherToMaybe _         = Nothing

-- restAreLits :: UrlChunks xs -> Bool
-- restAreLits Root = False
-- restAreLits (Cons ((:=) _) Root) = True
-- restAreLits (Cons ((:=) _) us)   = restAreLits us
-- restAreLits (Cons _ _) = False
