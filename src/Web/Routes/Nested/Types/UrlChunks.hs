{-# LANGUAGE
    KindSignatures
  , GADTs
  , RankNTypes
  , TypeOperators
  , DataKinds
  #-}

module Web.Routes.Nested.Types.UrlChunks where

import Data.Attoparsec.Text
import qualified Data.Text as T


-- | Constrained to AttoParsec & T.Text
data EitherUrlChunk (x :: Maybe *) where
  (:=) :: T.Text             -> EitherUrlChunk 'Nothing
  (:~) :: (T.Text, Parser r) -> EitherUrlChunk ('Just r)

-- | Container when defining route paths
data UrlChunks (xs :: [Maybe *]) where
  Cons :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs) -- unpacks left-to-right
  Root  :: UrlChunks '[]

-- `foldChunks :: (forall mx. EitherUrlChunk mx -> acc -> acc) -> acc -> UrlChunks xs -> acc
-- `foldChunks f i Root = i
-- `foldChunks f i (Cons u us) = foldChunks f (f u i) us
-- `
-- `appendChunks :: UrlChunks xs -> UrlChunks ys -> UrlChunks (xs :++ ys)
-- `appendChunks Root ys = ys
-- `appendChunks (Cons x xs) ys = Cons x $ appendChunks xs ys
-- `
-- `lastChunk :: UrlChunks xs -> EitherUrlChunk (Last xs)
-- `lastChunk (Cons u Root) = u
-- `lastChunk (Cons u us)   = lastChunk us
-- `
-- `initChunks :: UrlChunks xs -> UrlChunks (Init xs)
-- `initChunks (Cons u Root) = Root
-- `initChunks (Cons u us)   = Cons u $ initChunks us

(</>) = Cons
(#) = Root
