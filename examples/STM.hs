{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
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
import Data.Time
import Data.Maybe

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Crypto.Random
import Crypto.Random.Types
import Crypto.Hash

import qualified Database.VCache as VC


-- | @sec@
data SecurityLayer = ShouldBeLoggedIn
                   | ShouldBeModerator
                   | ShouldBeAdmin
  deriving (Show, Eq, Ord)

-- | @errSym@
data SecurityError = SessionTimedOut
                   | NotCoolEnough
  deriving (Show, Eq, Ord)


checksumHeader :: HeaderName
checksumHeader = "x-security-token"

timestampHeader :: HeaderName
timestampHeader = "x-security-token-time"

data UserSession a = UserSession
  { userSessTime :: UTCTime
  , userSessNonce :: a
  } deriving (Show, Eq, Ord)

makeUserSession :: UTCTime
                -> BS.ByteString -- salt
                -> UserSession BS.ByteString
makeUserSession t s = UserSession t $ convert go
  where go :: Digest SHA256
        go = hash $ s -- TODO: <> ISO 8601

getUserSession :: Request -> Maybe (UserSession BS.ByteString)
getUserSession req = do
  let hs = requestHeaders req
  t <- lookup timestampHeader hs -- TODO: Parse Timestamp
  c <- lookup checksumHeader hs
  return $ UserSession undefined c


data AuthenticationError = NoAuthHeaders
                         | InvalidRequestNonce
                         | SessionNotInCache

authenticate :: ( Monad m
                , MonadError AuthenticationError m
                ) => Request -> m (UserSession BS.ByteString)
authenticate req = do
  reqUserSess <- note' NoAuthHeaders $ getUserSession req
  undefined
  where
    salt :: BS.ByteString
    salt = let go :: Digest SHA256
               go = hash ("something spooky" :: BS.ByteString)
           in convert go
    note' n m = fromMaybe (throwError n) $ pure <$> m


-- | Runs only when successful. Adds the new checksum data, and can use MonadIO
-- for getCurrentTime
setChecksum :: Monad m => BS.ByteString -> m (Response -> Response)
setChecksum d = return $ mapResponseHeaders (add checksumHeader d)
 where  -- TODO: getCurrentTime timestamp header
   add k v [] = [(k,v)]
   add k v ((k',v'):xs) | k == k' = (k,v):xs
                        | otherwise = (k',v'): add k v xs


-- | Test to see if the incoming request satisfies the layers of authentication
-- demanded. Fail with an error, or return a new checksum to save in the client.
newChecksum :: ( Monad m
               , MonadError SecurityError m
               , MonadIO m
               ) => Request -> [SecurityLayer] -> m BS.ByteString
newChecksum _ [] = return "" -- Should it be (Maybe newData)? :(
newChecksum req ss = do -- TODO: check if timestamp is recent, lookup key in STM queue
  (n :: BS.ByteString) <- liftIO $ getRandomBytes 256
  let n' = hash n :: Digest SHA256
      n'' = convert n' :: BS.ByteString
  return n''



main :: IO ()
main = run 3000 $ routeAuth newChecksum
                            setChecksum $ do
  handle (l "foo" </> o)
    (Just $ get $ text "foo!")    -- current
    $ Just $ do
      auth ShouldBeLoggedIn (\e -> get $ textStatus status501 "Unauthorized") $ do  -- children
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
