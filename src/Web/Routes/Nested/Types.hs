{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Web.Routes.Nested.Types where

import Data.Attoparsec.Text
import Web.Routes.Nested.VerbListener
import Web.Routes.Nested.FileExtListener
import Network.Wai
import qualified Data.Text as T
import Data.List.NonEmpty
import Data.Trie.Pred
import Unsafe.Coerce
import Data.Functor.Compose


-- | Constrained to AttoParsec & T.Text
data EitherUrlChunk (x :: Maybe *) where
  (:=) :: T.Text             -> EitherUrlChunk 'Nothing
  (:~) :: (T.Text, Parser r) -> EitherUrlChunk ('Just r)

type family PrependOnJust (mx :: Maybe *) (xs :: [*]) :: [*] where
  PrependOnJust ('Just x) xs = x ': xs
  PrependOnJust 'Nothing  xs =      xs

type family FromMaybe (mx :: Maybe *) :: [*] where
  FromMaybe ('Just x) = x ': '[]
  FromMaybe 'Nothing  =      '[]

data LorP = LitLP
          | PredLP
          | Neither

type family LitOrPred (mx :: Maybe *) :: LorP where
  LitOrPred ('Just x) = 'PredLP
  LitOrPred 'Nothing  = 'LitLP

-- | Container when defining route paths
data UrlChunks (xs :: [*]) (last :: LorP) where
  Cons :: UrlChunks xs last -> EitherUrlChunk mx -> UrlChunks (PrependOnJust mx xs) (LitOrPred mx)
  Root  :: UrlChunks '[] 'Neither

(</>) = Cons
(=|) u = Cons Root u


class FunctorN ff where
  fmapN :: (a -> b) -> ff a -> ff b

instance FunctorN ff => Functor ff where
  fmap = fmapN

instance (FunctorN f, Functor g, Functor f) => FunctorN (Compose g f) where
  fmapN = fmap

instance Functor f => FunctorN f where
  fmapN = fmap



type family Tail (xs :: [*]) :: [*] where
  Tail (x ': xs) = xs

buildSingleton :: UrlChunks xs last
               -> ExpectArity xs a
               -> PredTrie T.Text T.Text a
buildSingleton chunks r | restAreLits chunks = Rest (toNE (unsafeCoerce chunks :: UrlChunks '[] last)) (unsafeCoerce r :: a)
                        | otherwise = case chunks of
                                        (Cons us   ((:=) t))      -> More t Nothing $ fromList [buildSingleton (unsafeCoerce us :: UrlChunks xs last) (unsafeCoerce r)]
                                        (Cons Root ((:~) (q,q'))) -> Pred (q,iResultToMaybe . parse q') (Just r) []
                                        (Cons us   ((:~) (q,q'))) -> Pred (q,iResultToMaybe . parse q') Nothing  [buildSingleton (unsafeCoerce us) (unsafeCoerce r)]
  where
    restAreLits :: UrlChunks xs last -> Bool
    restAreLits Root = False
    restAreLits (Cons Root ((:=) _)) = True
    restAreLits (Cons us   ((:=) _)) = restAreLits us
    restAreLits (Cons _ _) = False

    toNE :: UrlChunks '[] 'LitLP -> NonEmpty T.Text
    toNE (Cons Root ((:=) t)) = t :| []
    toNE (Cons us   ((:=) t)) = t :| (toList $ toNE $ unsafeCoerce us)
    toNE (Cons _ _) = error "can't make toNE of Nil or Parser"

    iResultToMaybe :: IResult T.Text r -> Maybe r
    iResultToMaybe (Done _ r) = Just r
    iResultToMaybe _          = Nothing


type family ExpectArity (xs :: [*]) (r :: *) :: * where
  ExpectArity '[] r = r
  ExpectArity (x ': xs) r = x -> (ExpectArity xs r)

type family ExpectContents z m a (lorp :: LorP) :: * where
  ExpectContents z m a 'PredLP = VerbListenerT z Response m a
  ExpectContents z m a 'LitLP  = VerbListenerT z (FileExts Response) m a

