{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DeriveDataTypeable
  , DataKinds
  #-}


module Main where

import Web.Routes.Nested
import Network.Wai.Trans
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import           Text.Regex
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import           Data.Attoparsec.Text hiding (match)
import Data.Monoid
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Catch


defApp :: Application
defApp _ respond = respond $ textOnlyStatus status404 "404 :("

main :: IO ()
main = run 3000 $
  (routeAuth authorize routes
  `catchMiddlewareT` handleUploadError
  `catchMiddlewareT` handleAuthError
  ) defApp
  where
    handleUploadError u = fileExtsToMiddleware $
      case u of
        NoChunkedBody  -> text "No chunked body allowed!"
        UploadTooLarge -> text "Upload too large"

    handleAuthError e = fileExtsToMiddleware $
      case e of
        NeedsAuth -> text "Authentication needed"

-- | A set of symbolic security roles
data AuthRole = AuthRole
  deriving (Show, Eq)

-- | The error thrown during authentication or authorization.
--
--   I would encourage other security-related error schemes:
--
--   - one data type for user authority errors - like when a logged-in user tries to access
--     the admin console
--   - one for types of failed authentication, during login (user doesn't exist,
--     incorrect password, etc.)
--   - one for malicious attacks - for instance if someone has tried (and failed) to login
--     5 times in the last minute, or multiple requests to the same resource
data AuthError = NeedsAuth
  deriving (Show, Eq, Typeable)

instance Exception AuthError

-- | Simple function that returns a response modification function - like
--   something that adds to the response headers - to maintain a session cookie,
--   for instance.
--
--   You can use 'Control.Monad.Catch.throwM' to throw arbitrary errors, but
--   you should also catch them with 'Network.Wai.Trans.catchMiddlewareT' to
--   avoid runtime exceptions.
authorize :: ( Monad m
             , MonadThrow m
             ) => Request -> [AuthRole] -> m (Response -> Response)
authorize req ss | null ss   = return id
                 | otherwise = throwM NeedsAuth

-- | The error thrown during the uploading function
data UploadError = NoChunkedBody
                 | UploadTooLarge
  deriving (Show, Typeable)

instance Exception UploadError


routes :: ( MonadIO m
          , MonadThrow m
          ) => RoutableT AuthRole m ()
routes = do
  matchHere (action rootHandle)
  matchGroup ("foo" </> o_) $ do
    matchHere (action fooHandle)
    auth AuthRole DontProtectHere
    match ("bar" </> o_) (action barHandle)
    match (p_ "double" double </> o_) doubleHandle
  match emailRoute emailHandle
  match ("baz" </> o_) (action bazHandle)
  matchAny (action notFoundHandle)
  where
    rootHandle :: MonadIO m => ActionT m ()
    rootHandle =
      get $ do
        text "Home"
        json ("Home" :: T.Text)

    -- `/foo`
    fooHandle :: MonadIO m => ActionT m ()
    fooHandle = get $ text "foo!"

    -- `/foo/bar`
    barHandle :: MonadIO m => ActionT m ()
    barHandle =
      get $ do
        text "bar!"
        json ("json bar!" :: T.Text)

    -- `/foo/1234e12`, uses attoparsec
    doubleHandle :: MonadIO m => Double-> MiddlewareT m
    doubleHandle d = action $ get $ text $ LT.pack (show d) <> " foos"

    -- `/athan@foo.com`
    emailRoute :: UrlChunks '[ 'Just [String] ]
    emailRoute = r_ "email" (mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o_

    emailHandle :: MonadIO m => [String] -> MiddlewareT m
    emailHandle e = action $ get $ text $ LT.pack (show e) <> " email"

    -- `/baz
    bazHandle :: ( MonadIO m
                 , MonadThrow m
                 ) => ActionT m ()
    bazHandle = do
      get $ text "baz!"
      post uploader $ text "uploaded!"
      where
        uploader :: (MonadIO m, MonadThrow m) => Request -> m ()
        uploader req =
          case requestBodyLength req of
                 ChunkedBody               -> throwM NoChunkedBody
                 KnownLength l | l > 1024  -> throwM UploadTooLarge
                               | otherwise -> liftIO $ print =<< strictRequestBody req


    notFoundHandle :: MonadIO m => ActionT m ()
    notFoundHandle = get $ textStatus status404 "Not Found :("
