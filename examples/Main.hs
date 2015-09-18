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
             , MonadError AuthErr m
             ) => Request -> [AuthRole] -> m (Response -> Response)
-- authorize _ _ = return id -- uncomment to force constant authorization
authorize req ss | null ss   = return id
                 | otherwise = throwError NeedsAuth

defApp :: Application
defApp _ respond = respond $ textOnlyStatus status404 "404 :("

main :: IO ()
main =
  let app = routeAuth authorize $
              handle o (Just rootHandle) $ Just $ do
                handle fooRoute (Just fooHandle) $ Just $ do
                  auth AuthRole unauthHandle
                  handle barRoute    (Just barHandle)    Nothing
                  handle doubleRoute (Just doubleHandle) Nothing
                handle emailRoute (Just emailHandle) Nothing
                handle bazRoute (Just bazHandle) Nothing
                notFound o (Just notFoundHandle) Nothing
  in run 3000 $ app defApp
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
