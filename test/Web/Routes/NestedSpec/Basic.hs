{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}


module Web.Routes.NestedSpec.Basic where

import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import           Text.Regex
import qualified Data.Text.Lazy as LT
import           Data.Attoparsec.Text
import Data.Monoid
import Control.Monad.Except


data AuthRole = AuthRole deriving (Show, Eq)
data AuthErr = NeedsAuth deriving (Show, Eq)

-- | If you fail here and throw an AuthErr, then the user was not authorized to
-- under the conditions set by @ss :: [AuthRole]@, and based on the authentication
-- of that user's session from the @Request@ object. Note that we could have a
-- shared cache of authenticated sessions, by adding more constraints on @m@ like
-- @MonadIO@.
-- For instance, even if there are [] auth roles, we could still include a header/timestamp
-- pair to uniquely identify the guest. Or, we could equally change @Checksum ~ Maybe Token@,
-- so a guest just returns Nothing, and we could handle the case in @putAuth@ to
-- not do anything.
authorize :: ( Monad m
             ) => Request -> [AuthRole] -> m (Response -> Response, Maybe AuthErr)
-- authorize _ _ = return id -- uncomment to force constant authorization
authorize req ss | null ss   = return (id, Nothing)
                 | otherwise = return (id, Just NeedsAuth)

defApp :: Application
defApp _ respond = respond $ textOnlyStatus status404 "404 :("

app :: Application
app =
  let yoDawgIHeardYouLikeYoDawgsYo = routeActionAuth authorize routes
      routes = do
        hereAction rootHandle
        parent fooRoute $ do
          hereAction fooHandle
          auth AuthRole unauthHandle DontProtectHere
          handleAction barRoute    barHandle
          handleAction doubleRoute doubleHandle
        handleAction emailRoute emailHandle
        handleAction bazRoute bazHandle
        notFoundAction notFoundHandle
  in yoDawgIHeardYouLikeYoDawgsYo defApp
  where
    rootHandle = get $ text "Home"

    -- `/foo`
    fooRoute = l_ "foo" </> o_
    fooHandle = get $ text "foo!"

    -- `/foo/bar`
    barRoute = l_ "bar" </> o_
    barHandle = get $ do
      text "bar!"
      json ("json bar!" :: LT.Text)

    -- `/foo/1234e12`, uses attoparsec
    doubleRoute = p_ ("double", double) </> o_
    doubleHandle d = get $ text $ LT.pack (show d) <> " foos"

    -- `/athan@foo.com`
    emailRoute = r_ ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o_
    emailHandle e = get $ text $ LT.pack (show e) <> " email"

    -- `/baz`, uses regex-compat
    bazRoute = l_ "baz" </> o_
    bazHandle = do
      get $ text "baz!"
      let uploader req = do liftIO $ print =<< strictRequestBody req
                            return ()
          uploadHandle (Left Nothing)  = text "Upload Failed"
          uploadHandle (Left (Just _)) = text "Impossible - no errors thrown in uploader"
          uploadHandle (Right ())      = text "Woah! Upload content!"
      post uploader uploadHandle

    unauthHandle NeedsAuth = get $ textStatus status401 "Unauthorized!"
    notFoundHandle = get $ textStatus status404 "Not Found :("
