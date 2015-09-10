{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}


module Main where

import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Web.Routes.Nested
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT


main :: IO ()
main = run 3000 $ routeAuth (const True) -- always returns True as a checksum
                            (const id) -- don't affect the response
                            (return . null) $ do -- gives True checksum when no auth tokens are present
  handle (l "foo" </> o)
    (Just $ get $ text "foo!")    -- current
    $ Just $ do
      auth (return True) (get $ textStatus status501 "Unauthorized") $ do
        handle (l "bar" </> o)      -- children
          (Just $ get $ do          -- current
             text "bar!"
             json ("json bar!" :: LT.Text)
          ) Nothing                 -- children
        handle (p ("double", double) </> o)
          (Just $ \(d :: Double) -> -- current
             get $ text $ (LT.pack $ show d) <> " foos"
          ) Nothing                -- children
  handle (r ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o)
    (Just $ \(e :: [String]) ->   -- current
       get $ text $ (LT.pack $ show e) <> " email"
    ) Nothing                     -- chilren
