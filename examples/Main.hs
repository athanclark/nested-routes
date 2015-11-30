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


data AuthRole  = AuthRole deriving (Show, Eq)
data AuthError = NeedsAuth deriving (Show, Eq, Typeable)

instance Exception AuthError

-- | If you fail here and throw an AuthErr, then the user was not authorized to
-- under the conditions set by @ss :: [AuthRole]@, and based o_n the authentication
-- o_f that user's session from the @Request@ o_bject. Note that we could have a
-- shared cache o_f authenticated sessions, by adding more constraints o_n @m@ like
-- @MonadIO@.
-- For instance, even if there are [] auth roles, we could still include a header/timestamp
-- pair to uniquely identify the guest. o_r, we could equally change @Checksum ~ Maybe Token@,
-- so a guest just returns Nothing, and we could handle the case in @putAuth@ to
-- not do anything.
authorize :: ( Monad m
             , MonadThrow m
             ) => Request -> [AuthRole] -> m (Response -> Response)
authorize req ss | null ss   = return id
                 | otherwise = throwM NeedsAuth

defApp :: Application
defApp _ respond = respond $ textOnlyStatus status404 "404 :("

main :: IO ()
main = run 3000 $
  (routeAuth authorize routes
  `catchMiddlewareT` handleUploadError
  `catchMiddlewareT` handleAuthError
  ) defApp


handleUploadError :: ( MonadIO m
                     ) => UploadError -> MiddlewareT m
handleUploadError u = fileExtsToMiddleware $
  case u of
    NoChunkedBody  -> text "No chunked body allowed!"
    UploadTooLarge -> text "Upload too large"

handleAuthError :: ( MonadIO m
                   ) => AuthError -> MiddlewareT m
handleAuthError e = fileExtsToMiddleware $
  case e of
    NeedsAuth -> text "Authentication needed"


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
    match (p_ ("double", double) </> o_) doubleHandle
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
    emailRoute = r_ ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o_

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
