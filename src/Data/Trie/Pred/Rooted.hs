module Data.Trie.Pred.Rooted where

import           Data.Trie.Pred hiding (merge)
import qualified Data.Trie.Pred as P

import Data.Monoid
import Control.Applicative


data RootedPredTrie t p a =
  RootedPredTrie { root :: Maybe a
                 , children :: [PredTrie t p a] }

instance Eq t => Monoid (RootedPredTrie t p a) where
  mempty = RootedPredTrie Nothing []
  mappend = merge

lookup :: (Eq t) =>
            [t]
         -> RootedPredTrie t p a
         -> Maybe a
lookup [] (RootedPredTrie mx _) = mx
lookup ts (RootedPredTrie _ xs) = foldr (go ts) Nothing xs
  where
    go ts x Nothing = P.lookup ts x
    go ts x ma@(Just _) = ma

merge :: (Eq t) =>
         RootedPredTrie t p a
      -> RootedPredTrie t p a
      -> RootedPredTrie t p a
merge (RootedPredTrie mx xs) (RootedPredTrie my ys) =
  RootedPredTrie (getLast $ Last mx <> Last my) $
    foldr go [] $ xs ++ ys
  where
    go q [] = [q]
    go q (z:zs) | areDisjoint q z = q : z : zs
                | otherwise = P.merge q z : zs

