{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}


module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Session
import Network.HTTP.Types
import Web.Routes.Nested
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import Data.ByteArray (convert)

import Control.Concurrent.STM
import Crypto.Random
import Crypto.Hash

tokenHeader = "x-security-token" :: HeaderName


getChecksum = lookup tokenHeader . requestHeaders

setChecksum d = mapResponseHeaders (add tokenHeader d)
  where
    add k v [] = [(k,v)]
    add k v ((k',v'):xs) | k == k' = (k,v):xs
                         | otherwise = (k',v'): add k v xs

newChecksum _ [] = return Nothing
newChecksum req ss = do
  (n :: BS.ByteString) <- getRandomBytes 256
  let n' = hash n :: Digest SHA256
      n'' = convert n' :: BS.ByteString
  return $ Just n''




main :: IO ()
main = run 3000 $ routeAuth getChecksum
                            setChecksum
                            newChecksum $ do
  handle (l "foo" </> o)
    (Just $ get $ text "foo!")    -- current
    $ Just $ do
      auth (return ()) (get $ textStatus status501 "Unauthorized") $ do  -- children
        handle (l "bar" </> o)
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
