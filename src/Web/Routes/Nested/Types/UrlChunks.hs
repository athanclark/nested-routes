{-# LANGUAGE
    GADTs
  , DataKinds
  , RankNTypes
  , BangPatterns
  , TypeOperators
  , KindSignatures
  , OverloadedStrings
  #-}

{- |
Module      : Web.Routes.Nested
Copyright   : (c) 2015 Athan Clark

License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Web.Routes.Nested.Types.UrlChunks
  ( -- * Path Combinators
    o_
  , origin_
  , l_
  , literal_
  , f_
  , file_
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
import Control.Monad
import Control.Error (hush)



o_, origin_ :: UrlChunks '[]
o_ = origin_

-- | The /Origin/ chunk - the equivalent to @[]@
origin_ = Root


l_, literal_ :: T.Text -> EitherUrlChunk 'Nothing
l_ = literal_

-- | Match against a /Literal/ chunk
literal_ = Lit

f_, file_ :: T.Text -> EitherUrlChunk ('Just T.Text)
f_ = file_

-- | Removes file extension from the matched route
file_ f = pred_ f (\t -> t <$ guard (fst (T.breakOn "." t) == f))


p_, parse_ :: T.Text -> Parser r -> EitherUrlChunk ('Just r)
p_ = parse_

-- | Match against a /Parsed/ chunk, with <https://hackage.haskell.org/package/attoparsec attoparsec>.
parse_ i q = pred_ i (hush . parseOnly q)


r_, regex_ :: T.Text -> Regex -> EitherUrlChunk ('Just [String])
r_ = regex_

-- | Match against a /Regular expression/ chunk, with <https://hackage.haskell.org/package/regex-compat regex-compat>.
regex_ i q = pred_ i (matchRegex q . T.unpack)

-- | Match with a predicate against the url chunk directly.
pred_ :: T.Text -> (T.Text -> Maybe r) -> EitherUrlChunk ('Just r)
pred_ = Pred


-- | Constrained to AttoParsec, Regex-Compat and T.Text
data EitherUrlChunk (x :: Maybe *) where
  Lit  { litChunk :: {-# UNPACK #-} !T.Text
       } :: EitherUrlChunk 'Nothing
  Pred { predTag  :: {-# UNPACK #-} !T.Text
       , predPred :: !(T.Text -> Maybe r)
       } :: EitherUrlChunk ('Just r)

-- | Use raw strings instead of prepending @l@
instance x ~ 'Nothing => IsString (EitherUrlChunk x) where
  fromString = literal_ . T.pack


-- | Container when defining route paths
data UrlChunks (xs :: [Maybe *]) where
  Cons :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs) -- stack is left-to-right
  Root :: UrlChunks '[]



-- | Prefix a routable path by more predicative lookup data.
(</>) :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs)
(</>) = Cons

infixr 9 </>
