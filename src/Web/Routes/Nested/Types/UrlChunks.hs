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

l :: T.Text -> EitherUrlChunk 'Nothing
l = (:=)

p :: (T.Text, Parser r) -> EitherUrlChunk ('Just r)
p = (:~)

-- | Container when defining route paths
data UrlChunks (xs :: [Maybe *]) where
  Cons :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs) -- stack is left-to-right
  Root  :: UrlChunks '[]

(</>) :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs)
(</>) = Cons

infixr 9 </>

o :: UrlChunks '[]
o = Root
