{-# LANGUAGE
    GADTs
  , DataKinds
  , RankNTypes
  , TypeOperators
  , OverloadedStrings
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

{- |
Module      : Web.Routes.Nested.Match
Copyright   : (c) 2015, 2016, 2017, 2018 Athan Clark

License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Web.Routes.Nested.Match
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
  , -- ** Path Types
    EitherUrlChunk
  , UrlChunks
  , ToUrlChunks (..)
  ) where

import Prelude              hiding (pred)
import Data.Attoparsec.Text (Parser, parseOnly)
import Text.Regex           (Regex, matchRegex)
import qualified Data.Text  as T
import Control.Monad        (guard)
import Control.Error        (hush)
import Data.Trie.Pred       (PathChunk, PathChunks, pred, nil, only, (./))


o_, origin_ :: UrlChunks '[]
o_ = origin_

-- | The /Origin/ chunk - the equivalent to @[]@
origin_ = nil


l_, literal_ :: T.Text -> EitherUrlChunk 'Nothing
l_ = literal_

-- | Match against a /Literal/ chunk
literal_ = only

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
pred_ = pred


-- | Constrained to AttoParsec, Regex-Compat and T.Text
type EitherUrlChunk = PathChunk T.Text


-- | Container when defining route paths
type UrlChunks = PathChunks T.Text



-- | Prefix a routable path by more predicative lookup data.
(</>) :: EitherUrlChunk mx -> UrlChunks xs -> UrlChunks (mx ': xs)
(</>) = (./)

infixr 9 </>



class ToUrlChunks a xs | a -> xs where
  toUrlChunks :: a -> UrlChunks xs
