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
  , AllowAmbiguousTypes
  #-}

module Web.Routes.Nested.Types where

import Data.Attoparsec.Text
import Web.Routes.Nested.Types.UrlChunks
import Web.Routes.Nested.VerbListener
import Web.Routes.Nested.FileExtListener
import Network.Wai
import qualified Data.Text as T
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Trie.Pred.Unified
import Data.Trie.Pred.Unified.Tail
import Unsafe.Coerce
import Data.Functor.Compose
import Data.Singletons.Prelude.List
import Data.Proxy


-- | Must be a non-empty UrlChunks
-- singleton :: UrlChunks xs
--           -> ExpectArity xs a
--           -> RUPTrie T.Text a
-- singleton Root   r = Rooted (Just r) []
-- singleton chunks r | restAreLits chunks = litSingleton (toL chunks) r
--                    | otherwise = Rooted Nothing [singletonTail chunks r]
--   where
--     singletonTail :: UrlChunks xs -> ExpectArity xs a -> UPTrie T.Text a
--     singletonTail chunks r = case chunks of
--       (Cons ((:=) t)     Root) -> UMore t (Just r) []
--       (Cons ((:=) t)     us)   -> UMore t Nothing  [singletonTail us r]
--       (Cons ((:~) (t,q)) Root) -> UPred t (iResultToMaybe . parse q) (Just r) []
--       (Cons ((:~) (t,q)) us)   -> UPred t (iResultToMaybe . parse q) Nothing  [singletonTail us r]


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

-- instance Extrude (UrlChunks xs) trie0 trie1 => Extrude (UrlChunks (('Just r) ': xs)) trie1 trie2 where
--   extrude (Cons ((:~) (t,q)) us) r = UPred t (iResultToMaybe . parse q) Nothing [extrude us r]

--  extrude :: UrlChunks xs
--          -> RUPTrie T.Text (ExpectArity xs a)
--          -> RUPTrie T.Text a
--  extrude Root r = r
--  extrude chunks r@(Rooted mr rs) | restAreLits chunks = litExtrude (toL chunks) r
--                                  | otherwise = Rooted Nothing [extrudeTail (initChunks chunks) (makeFirst (Proxy :: Proxy xs) (lastChunk chunks) mr rs)]
--    where
--      makeFirst :: Proxy xs -> EitherUrlChunk mx -> Maybe (ExpectArity xs a) -> [UPTrie t (ExpectArity xs a)] -> UPTrie t a
--      makeFirst _ ((:=) t)     mr rs = UMore t mr rs
--      makeFirst _ ((:~) (t,q)) mr rs = UPred t (iResultToMaybe . parse q) mr rs
--
--      extrudeTail :: UrlChunks xs' -> UPTrie T.Text (ExpectArity xs' a) -> UPTrie T.Text a
--      extrudeTail chunks r = case chunks of
--        (Cons Root ((:=) t))     -> UMore t Nothing [r]
--        (Cons us   ((:=) t))     -> UMore t Nothing [extrudeTail us r]
--        (Cons Root ((:~) (t,q))) -> UPred t (iResultToMaybe . parse q) Nothing [r]
--        (Cons us   ((:~) (t,q))) -> UPred t (iResultToMaybe . parse q) Nothing [extrudeTail us r]


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

