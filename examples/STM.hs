{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}


module Main where

import STM.Auth
import STM.Templates

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Session
import Network.Wai.Parse
import Network.HTTP.Types
import Web.Routes.Nested
import Web.Cookie
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import Data.ByteArray (convert)
import Data.Time
import Data.Maybe
import Data.Function.Syntax

import Control.Concurrent.STM
import Control.Error.Util
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Crypto.Random
import Crypto.Random.Types
import Crypto.Hash


data LoginError a = InvalidLoginAttempt
                  | Success a


defApp :: Application
defApp _ respond = respond $ textOnlyStatus status404 "404 :("

main :: IO ()
main = do
  cacheVar <- newTVarIO IntMap.empty
  uIdVar <- newTVarIO 0
  salt <- do (n :: BS.ByteString) <- liftIO $ getRandomBytes 256
             let n' = hash n :: Digest SHA512
                 n'' = convert n' :: BS.ByteString
             return n''
  let app a r1 r2 = runReaderT (routeAuth authenticate routes (liftIO .* a) r1 r2) $ AuthEnv cacheVar uIdVar salt
      -- routes :: ( MonadReader AuthEnv m
      --           , MonadIO m
      --           ) => RoutesT (LoginError (UserSession BS.ByteString)) SecurityLayer AuthenticationError m ()
      routes = do
        handle o (Just rootHandle) Nothing
        handle fooRoute (Just fooHandle) $ Just $ do
          auth ShouldBeLoggedIn unauthHandle ProtectChildren
          handle barRoute    (Just barHandle)    Nothing
          handle doubleRoute (Just doubleHandle) Nothing
        handle emailRoute (Just emailHandle) Nothing
        handle bazRoute (Just bazHandle) Nothing
        handle loginRoute (Just loginHandle) Nothing
        handle logoutRoute (Just logoutHandle) $ Just $
          auth ShouldBeLoggedIn unauthHandle ProtectParent
        notFound o (Just notFoundHandle) Nothing
  run 3000 $ app defApp
  where
    rootHandle = get $ text "Home"

    -- `/foo`
    fooRoute = l "foo" </> o
    fooHandle = get $ text "foo!"

    -- `/foo/bar`
    barRoute = l "bar" </> o
    barHandle = get $ do
      text "bar!"
      json ("json bar!" :: LT.Text)

    -- `/foo/1234e12`
    doubleRoute = p ("double", double) </> o
    doubleHandle d = get $ text $ LT.pack (show d) <> " foos"

    -- `/athan@foo.com`
    emailRoute = r ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o
    emailHandle e = get $ text $ LT.pack (show e) <> " email"

    -- `/baz`
    bazRoute = l "baz" </> o
    bazHandle = do
      get $ text "baz!"
      let uploader req = do liftIO $ print =<< strictRequestBody req
                            return Nothing
          uploadHandle Nothing = text "Upload Failed"
          uploadHandle (Just _) = text "Woah! Upload content!"
      post uploader uploadHandle

    loginRoute = "login" </> o
    loginHandle = do
      get $ lucid loginPage
      postReq (login 128) loginResp
        where
          loginResp _ Nothing = textStatus status500 "Server malfunction :E"
          loginResp _ (Just InvalidLoginAttempt) = textStatusWith clearSessionResponse status403 "Bad login"
          loginResp _ (Just (Success u)) = textWith (mapResponseHeaders (++ makeSessionCookies u)) "logged-in"

    login s req =
      case requestBodyLength req of
        KnownLength b | b <= s -> do
          (ps,_) <- liftIO $ parseRequestBody (const $ const $ const $ return ()) req
          if firstIs (\(x,y) -> BS.null x || BS.null y) ps || length ps < 2
          then return $ Just InvalidLoginAttempt
          else do
            mSuccess <- runMaybeT $ do
              user <- hoistMaybe $ lookup "user" ps
              pass <- hoistMaybe $ lookup "password" ps
              liftIO $ putStrLn $ "<~!~> Attempted Login: " ++ show user ++ ", " ++ show pass ++ "\n"
              nUserSess <- newUserSession
              lift $ writeUserSession nUserSess
              return $ Success nUserSess
            return $ Just $ fromMaybe InvalidLoginAttempt mSuccess
        _ -> return $ Just InvalidLoginAttempt
      where
        firstIs _ [] = True
        firstIs f (x:_) = f x


    logoutRoute = "logout" </> o
    logoutHandle = getReq $ \req -> do
      resp <- case getUserSession $ requestHeaders req of
        Nothing -> return "no session :s"
        Just userSession -> do
          lift $ deleteUserSession (userSessID userSession)
          return "logged-out"
      textWith clearSessionResponse resp

    unauthHandle SessionNotInCache   = get $ textStatusWith clearSessionResponse status401 "Unauthorized! - session not in cache"
    unauthHandle SessionTimedOut     = get $ textStatusWith clearSessionResponse status401 "Unauthorized! - session timed out"
    unauthHandle SessionMismatch     = get $ textStatusWith clearSessionResponse status401 "Unauthorized! - session mismatch"
    unauthHandle InvalidRequestNonce = get $ textStatusWith clearSessionResponse status401 "Unauthorized! - you're doing something sneaky!"
    unauthHandle NoSessionHeaders    = get $ textStatusWith clearSessionResponse status401 "Unauthorized! - are you sure you're logged in?"
    notFoundHandle = get $ textStatus status404 "Not Found :("
