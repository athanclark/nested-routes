{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}


module Main where

import Network.Wai.Handler.Warp
import Web.Routes.Nested
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT


fooLoc = l "foo" </> o
doubLoc = p ("double", double) </> o
emailLoc = r ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o

main = run 3000 $ route $ do
  handle fooLoc
    (Just $ get $ text "foo!")    -- current
    (Just $ do                    -- children
      handle (l "bar" </> o)
        (Just $ get $ do          -- current
           text "bar!"
           json ("json bar!" :: LT.Text)
        ) Nothing                 -- children
      handle doubLoc
        (Just $ \(d :: Double) -> -- current
           get $ text $ (LT.pack $ show d) <> " foos"
        ) Nothing)                -- children
  handle emailLoc
    (Just $ \(e :: [String]) ->   -- current
       get $ text $ (LT.pack $ show e) <> " email"
    ) Nothing                     -- chilren
