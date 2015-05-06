{-# LANGUAGE
    GADTs
  , TypeOperators
  , TypeFamilies
  , KindSignatures
  , DataKinds
  , RankNTypes
  , FlexibleInstances
  , UndecidableInstances
  , OverlappingInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  , ConstraintKinds
  #-}

module Web.Routes.Nested.Types
  ( Singleton (..)
  , Extend (..)
  , Extrude (..)
  , OnlyJusts
  , eitherToMaybe
  , restAreLits
  , ToNE (..)
  , ToL (..)
  , module Web.Routes.Nested.Types.UrlChunks
  ) where

import Data.Attoparsec.Text
import Text.Regex
import Web.Routes.Nested.Types.UrlChunks
import qualified Data.Text as T
import           Data.List.NonEmpty
import Data.Trie.Pred.Unified


type family OnlyJusts (xs :: [Maybe *]) :: [*] where
  OnlyJusts '[] = '[]
  OnlyJusts ('Nothing  ': xs) = OnlyJusts xs
  OnlyJusts (('Just x) ': xs) = x ': OnlyJusts xs

class Singleton chunks a trie | chunks a -> trie where
  singleton :: chunks -> a -> trie

instance Singleton (UrlChunks '[]) a (RUPTrie T.Text a) where
  singleton Root r = Rooted (Just r) []

instance ( Singleton (UrlChunks xs) a trie0
         , Extend (EitherUrlChunk x) trie0 trie1 )=>
         Singleton (UrlChunks (x ': xs)) a trie1 where
  singleton (Cons u us) r = extend u (singleton us r)


class Extend eitherUrlChunk child result | eitherUrlChunk child -> result where
  extend :: eitherUrlChunk -> child -> result

instance Extend (EitherUrlChunk  'Nothing) (RUPTrie T.Text a) (RUPTrie T.Text a) where
  extend ((:=) t) (Rooted mx xs) = Rooted Nothing [UMore t mx xs]

instance Extend (EitherUrlChunk ('Just r)) (RUPTrie T.Text (r -> a)) (RUPTrie T.Text a) where
  extend ((:~) (t,q)) (Rooted mx xs) = Rooted Nothing [UPred t (eitherToMaybe . parseOnly q) mx xs]
  extend ((:*) (t,q)) (Rooted mx xs) = Rooted Nothing [UPred t (matchRegex q . T.unpack)     mx xs]


class Extrude chunks start result | chunks start -> result where
  extrude :: chunks -> start -> result

instance Extrude (UrlChunks '[]) (RUPTrie T.Text a) (RUPTrie T.Text a) where
  extrude Root r = r

instance ( Extrude (UrlChunks xs) trie0 trie1
         , Extend (EitherUrlChunk x) trie1 trie2 ) => Extrude (UrlChunks (x ': xs)) trie0 trie2 where
  extrude (Cons u us) r = extend u (extrude us r)


eitherToMaybe :: Either String r -> Maybe r
eitherToMaybe (Right r) = Just r
eitherToMaybe _         = Nothing

restAreLits :: UrlChunks xs -> Bool
restAreLits Root = False
restAreLits (Cons ((:=) _) Root) = True
restAreLits (Cons ((:=) _) us)   = restAreLits us
restAreLits (Cons _ _) = False


class ToNE chunks where
  toNE :: chunks -> NonEmpty T.Text

instance ToNE (UrlChunks ('Nothing ': '[])) where
  toNE (Cons ((:=) t) Root) = t :| []

instance ToNE (UrlChunks xs) => ToNE (UrlChunks ('Nothing ': xs)) where
  toNE (Cons ((:=) t) us) = t :| (toList $ toNE us)


class ToL chunks where
  toL :: chunks -> [T.Text]

instance ToL (UrlChunks '[]) where
  toL Root = []

instance ToL (UrlChunks xs) => ToL (UrlChunks ('Nothing ': xs)) where
  toL (Cons ((:=) t) us) = t : toL us

