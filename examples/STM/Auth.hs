{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module STM.Auth where

import Network.Wai.Trans
import Network.HTTP.Types
import Web.Cookie
import Data.Attoparsec.Text
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import Blaze.ByteString.Builder (toByteString)
import qualified Data.IntMap as IntMap
import Data.ByteString.UTF8 (fromString)
import Data.ByteArray (convert)
import Data.Time
import Data.Time.ISO8601
import Data.Maybe

import Control.Concurrent.STM
import Control.Error.Util
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
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
                              , setCookieMaxAge = Just 60
                              , setCookieValue = fromString $ show i }
      , toByteString $
        renderSetCookie $ def { setCookieName = sessionTime
                              , setCookieMaxAge = Just 60
                              , setCookieValue = T.encodeUtf8 $ T.pack $ formatISO8601 t }
      , toByteString $
        renderSetCookie $ def { setCookieName = sessionNonce
                              , setCookieMaxAge = Just 60
                              , setCookieValue = c }
      ]

clearSessionResponse :: Response -> Response
clearSessionResponse = mapResponseHeaders $
  filter $ \(a,b) ->
    let cookieName = setCookieName $ parseSetCookie b
    in not $ a == "Set-Cookie" && ( cookieName == sessionId
                                 || cookieName == sessionTime
                                 || cookieName == sessionNonce )

clearSessionRequest :: Request -> Request
clearSessionRequest req =
  req {requestHeaders = go $ requestHeaders req}
  where
    go = filter $ \(a,b) ->
      let cookies = parseCookies b
          hasSession (k,_) = k == sessionId || k == sessionTime || k == sessionNonce
      in not $ a == "Cookie" && any hasSession cookies

getUserSession :: RequestHeaders -> Maybe (UserSession BS.ByteString)
getUserSession hs =
  let (mi,mt,mc) = foldr go (Nothing,Nothing,Nothing) hs
  in UserSession <$> mi <*> mt <*> mc
  where
    go (k,c) (mi,mt,mc) | k == "Cookie" =
        let cs = parseCookies c
            go' (k',c') (mi',mt',mc')
              | k' == sessionId = (hush $ parseOnly integer $ T.decodeUtf8 c', mt', mc')
              | k' == sessionTime = (mi', parseISO8601 $ T.unpack $ T.decodeUtf8 c', mc')
              | k' == sessionNonce = (mi', mt', Just c')
              | otherwise = (mi',mt',mc')
        in foldr go' (mi,mt,mc) cs
      | otherwise = (mi,mt,mc)


integer :: Parser Int
integer = do
  xs <- double
  return $ floor xs



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
  return $ UserSession uId now $ BS.encode $ convert $ go now salt
  where
    go :: UTCTime -> BS.ByteString -> Digest SHA512
    go now salt = hash $ salt <> fromString (formatISO8601 now)


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
      old' = BS.encode $ convert old :: BS.ByteString
  return $ c == old'


data AuthenticationError = NoSessionHeaders
                         | InvalidRequestNonce
                         | SessionNotInCache
                         | SessionMismatch
                         | SessionTimedOut


authenticate :: ( MonadIO m
                , MonadReader AuthEnv m
                ) => Request -> [SecurityLayer] -> m (Response -> Response, Maybe AuthenticationError)
authenticate _ [] = return (id, Nothing)
authenticate req _ = flip runStateT Nothing $ do
  liftIO $ putStrLn "<!> Auth Attempt <!>"
  liftIO $ putStrLn "--- Headers ----------- \n" >> print (requestHeaders req) >> putStrLn "\n"
  let mUserSession = getUserSession $ requestHeaders req
  put $ Just NoSessionHeaders <* mUserSession
  maybe (return id) hasUserSession mUserSession

hasUserSession userSession = do
  liftIO $ putStrLn "--- User Session ------ \n" >> print userSession >> putStrLn "\n"
  cacheKey <- authEnvCache <$> ask
  cache <- liftIO $ readTVarIO cacheKey
  liftIO $ putStrLn "--- Cache ------------- \n" >> print cache >> putStrLn "\n"
  mCachedSession <- lookupUserSession (userSessID userSession)
  put $ Just SessionNotInCache <* mCachedSession
  maybe (return id) (hasCachedSession userSession) mCachedSession

hasCachedSession userSession (UserSession i cachedTime cachedNonce) = do
  newUserSess' <- newUserSession
  let newUserSess = newUserSess' {userSessID = i}
  userSessionIsValid <- checkUserSession userSession
  case () of
    () | diffUTCTime (userSessTime newUserSess) cachedTime >= 60 -> do
           deleteUserSession i
           put $ Just SessionTimedOut
           return id
       | cachedNonce /= userSessNonce userSession -> do
           put $ Just SessionMismatch
           return id
       | not userSessionIsValid -> do
           put $ Just InvalidRequestNonce
           return id
       | otherwise -> do
           writeUserSession newUserSess
           return $ mapResponseHeaders (++ makeSessionCookies newUserSess)
                      . clearSessionResponse


note' :: MonadError e m => e -> Maybe a -> m a
note' e mx = fromMaybe (throwError e) $ pure <$> mx


insert :: Eq k => k -> a -> [(k,a)] -> [(k,a)]
insert k v [] = [(k,v)]
insert k v ((k',v'):xs) | k == k' = (k,v):xs
                        | otherwise = (k',v'): insert k v xs
