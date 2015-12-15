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

import           Web.Routes.Nested.Types.UrlChunks
import qualified Data.Text as T
import           Data.Trie.Pred
import           Data.Trie.Pred.Step
import qualified Data.Trie.HashMap as HT
import qualified Data.HashMap.Lazy as HM


-- | Convenience type-level function for removing 'Nothing's from a type list.
type family CatMaybes (xs :: [Maybe *]) :: [*] where
  CatMaybes '[]               = '[]
  CatMaybes ('Nothing  ': xs) =      CatMaybes xs
  CatMaybes (('Just x) ': xs) = x ': CatMaybes xs

-- | Creates a string of nodes - a trie with a width of 1.
class Singleton chunks a trie | chunks a -> trie where
  singleton :: chunks -> a -> trie

-- Basis
instance Singleton (UrlChunks '[]) a (RootedPredTrie T.Text a) where
  singleton Root r = RootedPredTrie (Just r) emptyPT

-- Successor
instance ( Singleton (UrlChunks xs) a trie0
         , Extend (EitherUrlChunk x) trie0 trie1
         ) => Singleton (UrlChunks (x ': xs)) a trie1 where
  singleton (Cons u us) r = extend u $! singleton us r


-- | Turn a list of tries (@Rooted@) into a node with those children
class Extend eitherUrlChunk child result | eitherUrlChunk child -> result where
  extend :: eitherUrlChunk -> child -> result

-- | Literal case
instance Extend (EitherUrlChunk  'Nothing) (RootedPredTrie T.Text a) (RootedPredTrie T.Text a) where
  extend (Lit t) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie (HT.HashMapStep $! HM.singleton t (HT.HashMapChildren mx $ Just xs)) mempty

-- | Existentially quantified case
instance Extend (EitherUrlChunk ('Just r)) (RootedPredTrie T.Text (r -> a)) (RootedPredTrie T.Text a) where
  extend (Pred i q) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie mempty (PredSteps [PredStep i q mx xs])


-- | @FoldR Extend start chunks ~ result@
class Extrude chunks start result | chunks start -> result where
  extrude :: chunks -> start -> result

-- Basis
instance Extrude (UrlChunks '[]) (RootedPredTrie T.Text a) (RootedPredTrie T.Text a) where
  extrude Root r = r

-- Successor
instance ( Extrude (UrlChunks xs) trie0 trie1
         , Extend (EitherUrlChunk x) trie1 trie2 ) => Extrude (UrlChunks (x ': xs)) trie0 trie2 where
  extrude (Cons u us) r = extend u $! extrude us r

