{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Web.Routes.Nested.Types where

import Data.Attoparsec.Text
import Web.Routes.Nested.VerbListener
import Web.Routes.Nested.FileExtListener
import Network.Wai
import qualified Data.Text as T


-- | Constrained to AttoParsec & T.Text
data EitherUrlChunk (x :: Maybe *) where
  (:=) :: T.Text     -> EitherUrlChunk 'Nothing
  (:~) :: (Parser r) -> EitherUrlChunk ('Just r)

type family PrependOnJust (mx :: Maybe *) (xs :: [*]) :: [*] where
  PrependOnJust ('Just x) xs = x ': xs
  PrependOnJust 'Nothing  xs =      xs

type family FromMaybe (mx :: Maybe *) :: [*] where
  FromMaybe ('Just x) = x ': '[]
  FromMaybe 'Nothing  =      '[]

data LorP = Lit
          | Pred

type family LitOrPred (mx :: Maybe *) :: LorP where
  LitOrPred ('Just x) = 'Pred
  LitOrPred 'Nothing  = 'Lit

-- | Container when defining route paths
data UrlChunks (xs :: [*]) (last :: LorP) where
  Cons :: UrlChunks xs last -> EitherUrlChunk mx -> UrlChunks (PrependOnJust mx xs) (LitOrPred mx)
  Pure :: EitherUrlChunk mx -> UrlChunks (FromMaybe mx) (LitOrPred mx)


type family ExpectArity (xs :: [*]) (r :: *) :: * where
  ExpectArity '[] r = r
  ExpectArity (x ': xs) r = x -> (ExpectArity xs r)

type family ExpectContents z m a (lorp :: LorP) :: * where
  ExpectContents z m a 'Pred = VerbListenerT z Response m a
  ExpectContents z m a 'Lit  = VerbListenerT z (FileExts Response) m a

