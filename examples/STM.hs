{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}


module Main where

import STM.Auth
import STM.Templates

import Network.Wai.Trans
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import Web.Routes.Nested
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import Data.ByteArray (convert)

import Control.Concurrent.STM
import Control.Error.Util
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Crypto.Random
import Crypto.Hash


data LoginError = InvalidLoginAttempt

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
  let routedApp app req resp = runReaderT (routeActionAuth authenticate routes app req resp) $ AuthEnv cacheVar uIdVar salt
      routes = do
        hereAction rootHandle
        parent ("foo" </> o) $ do
          hereAction fooHandle
          auth ShouldBeLoggedIn unauthHandle ProtectChildren
          handleAction ("bar" </> o) barHandle
          handleAction doubleRoute doubleHandle
        handleAction emailRoute emailHandle
        handleAction ("baz" </> o) bazHandle
        handleAction ("login" </> o) loginHandle
        parent ("logout" </> o) $ do
          hereAction logoutHandle
          auth ShouldBeLoggedIn unauthHandle ProtectParent
        notFoundAction notFoundHandle
  run 3000 $ routedApp $ liftApplication defApp
  where
    rootHandle = get $ text "Home"

    -- `/foo`
    fooHandle = get $ text "foo!"

    -- `/foo/bar`
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
    bazHandle = do
      get $ text "baz!"
      let uploader req = do liftIO $ print =<< strictRequestBody req
                            return undefined
          uploadHandle (Left Nothing) = text "Upload Failed"
          uploadHandle (Left (Just _)) = text "Should never happen"
          uploadHandle (Right _) = text "Woah! Upload content!"
          -- Hidden flaw ^ UserSession & LoginError are only options
      post uploader uploadHandle

    loginHandle = do
      get $ lucid loginPage
      postReq (login 128) loginResp
        where
          loginResp _ (Left Nothing) = textStatus status500 "Server malfunction :E"
          loginResp _ (Left (Just InvalidLoginAttempt)) = textStatusWith clearSessionResponse status403 "Bad login"
          loginResp _ (Right u) = textWith (mapResponseHeaders (++ makeSessionCookies u)) "logged-in"

    login s req =
      case requestBodyLength req of
        KnownLength b | b <= s -> do
          (ps,_) <- liftIO $ parseRequestBody (const $ const $ const $ return ()) req
          if firstIs (\(x,y) -> BS.null x || BS.null y) ps || length ps < 2
          then throwError $ Just InvalidLoginAttempt
          else do
            mSuccess <- runMaybeT $ do
              user <- hoistMaybe $ lookup "user" ps
              pass <- hoistMaybe $ lookup "password" ps
              liftIO $ putStrLn $ "<~!~> Attempted Login: " ++ show user ++ ", " ++ show pass ++ "\n"
              nUserSess <- newUserSession
              lift $ writeUserSession nUserSess
              return nUserSess
            note' (Just InvalidLoginAttempt) mSuccess
        _ -> throwError $ Just InvalidLoginAttempt
      where
        firstIs _ [] = True
        firstIs f (x:_) = f x


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

