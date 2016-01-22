{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DeriveGeneric
  #-}


module Web.Routes.NestedSpec.Basic where

import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import           Text.Regex
import qualified Data.Text.Lazy as LT
import           Data.Attoparsec.Text hiding (match)
import Data.Monoid
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Generics


data AuthRole = AuthRole deriving (Show, Eq)
data AuthErr = NeedsAuth deriving (Show, Eq, Generic)

instance Exception AuthErr

-- | If you fail here and throw an AuthErr, then the user was not authorized to
--   under the conditions set by @ss :: [AuthRole]@, and based on the authentication
--   of that user's session from the @Request@ object. Note that we could have a
--   shared cache of authenticated sessions, by adding more constraints on @m@ like
--   @MonadIO@.
--   For instance, even if there are [] auth roles, we could still include a header/timestamp
--   pair to uniquely identify the guest. Or, we could equally change @Checksum ~ Maybe Token@,
--   so a guest just returns Nothing, and we could handle the case in @putAuth@ to
--   not do anything.
authorize :: ( MonadThrow m
             ) => Request -> [AuthRole] -> m ()
-- authorize _ _ = return id -- uncomment to force constant authorization
authorize req ss | null ss   = return ()
                 | otherwise = throwM NeedsAuth

defApp :: Application
defApp _ respond = respond $
  textOnly "404 :(" status404 []

successMiddleware :: Middleware
successMiddleware _ _ respond = respond $ textOnly "200!" status200 []

app :: Application
app =
  let yoDawgIHeardYouLikeYoDawgsYo =
        (routeAuth authorize routes) `catchMiddlewareT` unauthHandle
      routes = do
        matchHere successMiddleware
        matchGroup fooRoute $ do
          matchHere successMiddleware
          auth AuthRole DontProtectHere
          match barRoute    successMiddleware
        match doubleRoute (\_ -> successMiddleware)
        match emailRoute (\_ -> successMiddleware)
        match bazRoute successMiddleware
  in yoDawgIHeardYouLikeYoDawgsYo defApp
  where
    -- `/foo`
    fooRoute = l_ "foo" </> o_

    -- `/foo/bar`
    barRoute = l_ "bar" </> o_

    -- `/foo/1234e12`, uses attoparsec
    doubleRoute = p_ "double" double </> o_

    -- `/athan@foo.com`
    emailRoute = r_ "email" (mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o_

    -- `/baz`, uses regex-compat
    bazRoute = l_ "baz" </> o_

unauthHandle :: AuthErr -> Middleware
unauthHandle NeedsAuth _ _ respond = respond $
  textOnly "Unauthorized!" status401 []
