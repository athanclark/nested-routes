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

import Debug.Trace


type Checksum = ()
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
newAuth :: ( Monad m
           , MonadError AuthErr m
           ) => Request -> [AuthRole] -> m Checksum
newAuth req ss | null ss   = return ()
               | otherwise = throwError NeedsAuth

-- | This is ran only on success - that the user has been authenticated again
-- and we can continue placing the checksum in a session cookie. Best used with
-- @mapResponseHeaders@ and @getCurrentTime@ (for a timestamp). Or you could
-- include both in @Checksum@. Or you could make it @Maybe Checksum@, so you
-- don't always have to change the response, but still consider it a successful
-- request / response in terms of being authenticated and authorized (as a guest).
putAuth :: Monad m => Checksum -> m (Response -> Response)
putAuth d = return id


main :: IO ()
main = run 3000 $ routeAuth newAuth -- always returns True as a checksum
                            putAuth $ -- don't affect the response
  handle o (Just rootHandle) $ Just $ do
    handle fooRoute (Just fooHandle) $ Just $
      auth AuthRole unauthHandle $ do
        handle barRoute    (Just barHandle)    Nothing
        handle doubleRoute (Just doubleHandle) Nothing
    handle emailRoute (Just emailHandle) Nothing
    handle bazRoute (Just bazHandle) Nothing
    notFound o (Just notFoundHandle) Nothing
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
    bazHandle = get $ text "baz!"

    unauthHandle NeedsAuth = get $ textStatus status401 "Unauthorized!"
    notFoundHandle = get $ textStatus status404 "Not Found :("
