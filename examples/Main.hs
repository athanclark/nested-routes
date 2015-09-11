{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}


module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Web.Routes.Nested
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT
import Control.Monad.Error.Class



-- Checksums are ()
-- `sec` is also ()

data AuthErr = NeedsAuth

newAuth :: ( Monad m
           , MonadError AuthErr m
           ) => Request -> [()] -> m ()
newAuth req ss | null ss   = return () -- no auth needed!
               | otherwise = throwError NeedsAuth -- or you could fail
                -- if `old /=` something you generate from `req`.

putAuth :: Monad m => () -> m (Response -> Response)
putAuth d = return id


main :: IO ()
main = run 3000 $ routeAuth newAuth -- always returns True as a checksum
                            putAuth $ -- don't affect the response
  handle o (Just $ get $ text "Home") $ Just $ do
    handle (l "foo" </> o)
      (Just $ get $ text "foo!")    -- current
      $ Just $ do
        auth (return ()) (\NeedsAuth -> get $ textStatus status401 "Unauthorized >:c") $ do
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
    notFound o (Just $ get $ textStatus status404 "Not Found :(") Nothing
