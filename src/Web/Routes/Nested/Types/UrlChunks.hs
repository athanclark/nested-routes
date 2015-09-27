{-# LANGUAGE
    KindSignatures
  , GADTs
  , RankNTypes
  , TypeOperators
  , DataKinds
  #-}

module Web.Routes.Nested.Types.UrlChunks where

import Data.Attoparsec.Text
import Text.Regex
import Data.String (IsString (..))
import qualified Data.Text as T


-- | Constrained to AttoParsec, Regex-Compat and T.Text
data EitherUrlChunk (x :: Maybe *) where
  (:=) :: T.Text             -> EitherUrlChunk 'Nothing
  (:~) :: (T.Text, Parser r) -> EitherUrlChunk ('Just r)
  (:*) :: (T.Text, Regex)    -> EitherUrlChunk ('Just [String])

-- | Use raw strings instead of prepending @l@
instance x ~ 'Nothing => IsString (EitherUrlChunk x) where
  fromString = l . T.pack

-- | The /Origin/ chunk - the equivalent to @[]@
o :: UrlChunks '[]
o = Root

-- | Match against a /Literal/ chunk
l :: T.Text -> EitherUrlChunk 'Nothing
l = (:=)

-- | Match against a /Parsed/ chunk
p :: (T.Text, Parser r) -> EitherUrlChunk ('Just r)
p = (:~)

-- | Match against a /Regular expression/ chunk
r :: (T.Text, Regex) -> EitherUrlChunk ('Just [String])
r = (:*)

-- | Glue two chunks together
(</>) :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs)
(</>) = Cons

infixr 9 </>

-- | Container when defining route paths
data UrlChunks (xs :: [Maybe *]) where
  Cons :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs) -- stack is left-to-right
  Root  :: UrlChunks '[]
