module Data.Trie.Pred.Rooted where

import           Data.Trie.Pred hiding (merge)
import qualified Data.Trie.Pred as P

import Data.Monoid
import Control.Applicative


data RootedPredTrie t p c a =
  RootedPredTrie { root :: Maybe a
                 , children :: [PredTrie t p c a] }

instance Eq t => Monoid (RootedPredTrie t p c a) where
  mempty = RootedPredTrie Nothing []
  mappend = merge

lookupLR :: (Eq t, Functor c) =>
            [t]
         -> RootedPredTrie t p c a
         -> Maybe (Either (c a) a)
lookupLR [] (RootedPredTrie mx _) = Right <$> mx
lookupLR ts (RootedPredTrie _ xs) = foldr (go ts) Nothing xs
  where
    go ts x Nothing = P.lookupLR ts x
    go ts x ma@(Just _) = ma

merge :: (Eq t) =>
         RootedPredTrie t p c a
      -> RootedPredTrie t p c a
      -> RootedPredTrie t p c a
merge (RootedPredTrie mx xs) (RootedPredTrie my ys) =
  RootedPredTrie (getLast $ Last mx <> Last my) $
    foldr go [] $ xs ++ ys
  where
    go q [] = [q]
    go q (z:zs) | areDisjoint q z = q : z : zs
                | otherwise = P.merge q z : zs

