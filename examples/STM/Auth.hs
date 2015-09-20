{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module STM.Auth where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Session
import Network.HTTP.Types
import Web.Routes.Nested
import Web.Cookie
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import Blaze.ByteString.Builder (toByteString)
import qualified Data.IntMap as IntMap
import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteArray (convert)
import Data.Time
import Data.Time.ISO8601
import Data.Maybe
import Data.Default

import Control.Concurrent.STM
import Control.Monad.STM
import Control.Monad.Error.Class
import Control.Error.Util
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Crypto.Random
import Crypto.Random.Types
import Crypto.Hash


-- | @sec@
data SecurityLayer = ShouldBeLoggedIn
                   | ShouldBeModerator
                   | ShouldBeAdmin
  deriving (Show, Eq, Ord)

data AuthEnv = AuthEnv
  { authEnvCache   :: TVar (IntMap.IntMap (UTCTime, BS.ByteString))
  , authEnvFreshId :: TVar Int
  , authEnvSalt :: BS.ByteString
  }


sessionId :: BS.ByteString
sessionId = "security-id"

sessionNonce :: BS.ByteString
sessionNonce = "security-token"

sessionTime :: BS.ByteString
sessionTime = "security-token-time"

makeSessionCookies :: UserSession BS.ByteString -> RequestHeaders
makeSessionCookies (UserSession i t c) = repeat "Set-Cookie" `zip` cookies
  where
    cookies =
      [ toByteString $
        renderSetCookie $ def { setCookieName = sessionId
                              , setCookieValue = fromString $ show i }
      , toByteString $
        renderSetCookie $ def { setCookieName = sessionNonce
                              , setCookieValue = T.encodeUtf8 $ T.pack $ formatISO8601 t }
      , toByteString $
        renderSetCookie $ def { setCookieName = sessionTime
                              , setCookieValue = c }
      ]

getUserSession :: RequestHeaders -> Maybe (UserSession BS.ByteString)
getUserSession hs =
  let (mi,mt,mc) = foldr go (Nothing,Nothing,Nothing) hs
  in UserSession <$> mi <*> mt <*> mc
  where
    go (k,c) (mi,mt,mc) | k == "Set-Cookie" =
      let c' = parseSetCookie c
      in case setCookieName c' of
            name | name == sessionId    -> (hush $ parseOnly integer $ T.decodeUtf8 $ setCookieValue c', mt, mc)
                 | name == sessionNonce -> (mi, mt, Just $ setCookieValue c')
                 | name == sessionTime  -> (mi, parseISO8601 $ show $ setCookieValue c', mc)


integer :: Parser Int
integer = do
  xs <- many1 digit
  return $ fst $ foldr (\n (m,d) -> (m + (read [n]) * d, d+10)) (0,1) xs



data UserSession a = UserSession
  { userSessID    :: Int
  , userSessTime  :: UTCTime
  , userSessNonce :: a
  } deriving (Show, Eq, Ord)

newUserSession :: ( MonadIO m
                  , MonadReader AuthEnv m
                  ) => m (UserSession BS.ByteString)
newUserSession = do
  salt <- authEnvSalt <$> ask
  uIdKey <- authEnvFreshId <$> ask
  uId <- liftIO $ atomically $ do
    i <- readTVar uIdKey
    modifyTVar' uIdKey (+1)
    return i
  now <- liftIO getCurrentTime
  return $ UserSession uId now $ convert $ go now salt
  where
    go :: UTCTime -> BS.ByteString -> Digest SHA512
    go now salt = hash $ salt <> fromString (show now)


lookupUserSession :: ( MonadIO m
                     , MonadReader AuthEnv m
                     ) => Int -> m (Maybe (UserSession BS.ByteString))
lookupUserSession i = do
  cacheKey <- authEnvCache <$> ask
  liftIO $ atomically $ do
    cache <- readTVar cacheKey
    return $ do (t,c) <- IntMap.lookup i cache
                return $ UserSession i t c

deleteUserSession :: ( MonadIO m
                     , MonadReader AuthEnv m
                     ) => Int -> m ()
deleteUserSession i = do
  cacheKey <- authEnvCache <$> ask
  liftIO $ atomically $ modifyTVar' cacheKey (IntMap.delete i)


writeUserSession :: ( MonadIO m
                    , MonadReader AuthEnv m
                    ) => UserSession BS.ByteString -> m ()
writeUserSession (UserSession i t c) = do
  cacheKey <- authEnvCache <$> ask
  liftIO $ atomically $ modifyTVar' cacheKey $ IntMap.insert i (t, c)


checkUserSession :: MonadReader AuthEnv m =>
                    UserSession BS.ByteString
                 -> m Bool
checkUserSession (UserSession _ t c) = do
  salt <- authEnvSalt <$> ask
  let old = hash $ salt <> fromString (formatISO8601 t) :: Digest SHA512
      old' = convert old :: BS.ByteString
  return $ c == old'


data AuthenticationError = NoSessionHeaders
                         | InvalidRequestNonce
                         | SessionNotInCache
                         | SessionMismatch
                         | SessionTimedOut


authenticate :: ( MonadIO m
                , MonadError AuthenticationError m
                , MonadReader AuthEnv m
                ) => Request -> [SecurityLayer] -> m (Response -> Response)
authenticate _ [] = return id
authenticate req _ = do
  let sessions    = (\(a,b) -> parseSetCookie b) <$> requestHeaders req
  userSession        <- note' NoSessionHeaders $ getUserSession $ requestHeaders req
  userSessionIsValid <- checkUserSession userSession
  now                <- liftIO getCurrentTime
  -- cacheVar           <- authEnvCache <$> ask
  newUserSess        <- newUserSession
  mCachedSession     <- lookupUserSession (userSessID userSession)
  (UserSession i oldTime oldNonce) <- note' SessionNotInCache mCachedSession
  case () of
    () | diffUTCTime now oldTime >= 60         -> do deleteUserSession i
                                                     throwError SessionTimedOut
       | oldNonce /= userSessNonce userSession -> throwError SessionMismatch
       | not userSessionIsValid                -> throwError InvalidRequestNonce
       | otherwise -> do writeUserSession newUserSess
                         return $ mapResponseHeaders (++ makeSessionCookies newUserSess)




note' e mx = fromMaybe (throwError e) $ pure <$> mx


insert k v [] = [(k,v)]
insert k v ((k',v'):xs) | k == k' = (k,v):xs
                        | otherwise = (k',v'): insert k v xs
