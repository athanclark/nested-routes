{-# LANGUAGE
    KindSignatures
  , GADTs
  , RankNTypes
  , TypeOperators
  , DataKinds
  #-}

module Web.Routes.Nested.Types.UrlChunks
  ( -- * Path Combinators
    o_
  , origin_
  , l_
  , literal_
  , p_
  , parse_
  , r_
  , regex_
  , pred_
  , (</>)
  , -- * Path Types
    EitherUrlChunk (..)
  , UrlChunks (..)
  ) where

import Data.Attoparsec.Text
import Text.Regex
import Data.String (IsString (..))
import qualified Data.Text as T


-- | Constrained to AttoParsec, Regex-Compat and T.Text
data EitherUrlChunk (x :: Maybe *) where
  (:=) :: T.Text                      -> EitherUrlChunk 'Nothing
  (:~) :: (T.Text, T.Text -> Maybe r) -> EitherUrlChunk ('Just r)

-- | Use raw strings instead of prepending @l@
instance x ~ 'Nothing => IsString (EitherUrlChunk x) where
  fromString = literal_ . T.pack


o_, origin_ :: UrlChunks '[]
o_ = origin_

-- | The /Origin/ chunk - the equivalent to @[]@
origin_ = Root


l_, literal_ :: T.Text -> EitherUrlChunk 'Nothing
l_ = literal_

-- | Match against a /Literal/ chunk
literal_ = (:=)


p_, parse_ :: (T.Text, Parser r) -> EitherUrlChunk ('Just r)
p_ = parse_

-- | Match against a /Parsed/ chunk, with <https://hackage.haskell.org/package/attoparsec attoparsec>.
parse_ (i,q) = (:~) (i, eitherToMaybe . parseOnly q)


r_, regex_ :: (T.Text, Regex) -> EitherUrlChunk ('Just [String])
r_ = regex_

-- | Match against a /Regular expression/ chunk, with <https://hackage.haskell.org/package/regex-compat regex-compat>.
regex_ (i,q) = (:~) (i, matchRegex q . T.unpack)

-- | Match with a predicate against the url chunk directly.
pred_ :: (T.Text, T.Text -> Maybe r) -> EitherUrlChunk ('Just r)
pred_ = (:~)

-- | Prefix a routable path by more predicative lookup data.
(</>) :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs)
(</>) = Cons

infixr 9 </>

-- | Container when defining route paths
data UrlChunks (xs :: [Maybe *]) where
  Cons :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs) -- stack is left-to-right
  Root :: UrlChunks '[]


eitherToMaybe :: Either String r -> Maybe r
eitherToMaybe (Right r') = Just r'
eitherToMaybe _         = Nothing
