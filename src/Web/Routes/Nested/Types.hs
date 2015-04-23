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
  #-}

module Web.Routes.Nested.Types
  ( Singleton (..)
  , Extend (..)
  , Extrude (..)
  , iResultToMaybe
  , restAreLits
  , ToNE (..)
  , ToL (..)
  , ExpectArity
  , module Web.Routes.Nested.Types.UrlChunks
  ) where

import Data.Attoparsec.Text
import Web.Routes.Nested.Types.UrlChunks
import Web.Routes.Nested.VerbListener
import Web.Routes.Nested.FileExtListener
import Network.Wai
import qualified Data.Text as T
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Trie.Pred.Unified
import Data.Trie.Pred.Unified.Tail


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

instance Extend (EitherUrlChunk  'Nothing) (RUPTrie T.Text a)        (RUPTrie T.Text a) where
  extend ((:=) t) (Rooted mx xs) = Rooted Nothing [UMore t mx xs]

instance Extend (EitherUrlChunk ('Just r)) (RUPTrie T.Text (r -> a)) (RUPTrie T.Text a) where
  extend ((:~) (t,q)) (Rooted mx xs) = Rooted Nothing [UPred t (iResultToMaybe . parse q) mx xs]


class Extrude chunks start result | chunks start -> result where
  extrude :: chunks -> start -> result

instance Extrude (UrlChunks '[]) (RUPTrie T.Text a) (RUPTrie T.Text a) where
  extrude Root r = r

instance ( Extrude (UrlChunks xs) trie0 trie1
         , Extend (EitherUrlChunk x) trie1 trie2 ) => Extrude (UrlChunks (x ': xs)) trie0 trie2 where
  extrude (Cons u us) r = extend u (extrude us r)


iResultToMaybe :: IResult T.Text r -> Maybe r
iResultToMaybe (Done _ r) = Just r
iResultToMaybe _          = Nothing

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


type family ExpectArity (xs :: [Maybe *]) (r :: *) :: * where
  ExpectArity '[] r = r
  ExpectArity ('Nothing  ': xs) r = ExpectArity xs r
  ExpectArity (('Just x) ': xs) r = x -> (ExpectArity xs r)

