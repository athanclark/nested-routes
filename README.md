![Logo](https://cdn.rawgit.com/athanclark/nested-routes-website/master/static/images/logo.svg)

[![Stories in Ready](https://badge.waffle.io/athanclark/nested-routes.png?label=ready&title=Ready)](https://waffle.io/athanclark/nested-routes)
nested-routes
=============

> Yet-Another WAI Router.

A method to writing Wai responses

This library attempts to make it easier to write nice Wai response handlers
by giving us a Sinatra/
[Scotty](https://hackage.haskell.org/package/scotty)-like syntax for declaring HTTP-verb oriented
routes, in addition to file-extension handling and rose-tree like composition.
Not only do we have literal route specification, like
[Scotty](https://hackage.haskell.org/package/scotty) &
[Spcok](https://hackage.haskell.org/package/spock), but we
can also embed
[Attoparsec](https://hackage.haskell.org/package/attoparsec)
parsers /directly/ in our routes, with our handlers
reflecting their results. As an example:

```haskell
{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}


module Main where

import Web.Routes.Nested
import Network.Wai.Trans
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import           Text.Regex
import qualified Data.Text.Lazy as LT
import           Data.Attoparsec.Text
import Data.Monoid
import Control.Monad.Except


data AuthRole = AuthRole deriving (Show, Eq)
data AuthErr = NeedsAuth deriving (Show, Eq)

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
             ) => Request -> [AuthRole] -> m (Response -> Response, Maybe AuthErr)
-- authorize _ _ = return id -- uncomment to force constant authorization
authorize req ss | null ss   = return (id, Nothing)
                 | otherwise = return (id, Just NeedsAuth)                                                                                                              
                                                                                                                                                                        
defApp :: Application                                                                                                                                                   
defApp _ respond = respond $ textOnlyStatus status404 "404 :("

main :: IO ()
main =
  let app = routeActionAuth authorize routes
      routes = do
        hereAction rootHandle
        parent ("foo" </> o_) $ do
          hereAction fooHandle
          auth AuthRole unauthHandle DontProtectHere
          handleAction ("bar" </> o_) barHandle
          handleAction (p_ ("double", double) </> o_) doubleHandle
        handleAction emailRoute emailHandle
        handleAction ("baz" </> o_) bazHandle
        notFoundAction notFoundHandle
  in run 3000 $ app defApp
  where
    rootHandle = get $ text "Home"

    -- `/foo`
    fooHandle = get $ text "foo!"

    -- `/foo/bar`
    barHandle = get $ do
      text "bar!"
      json ("json bar!" :: LT.Text)

    -- `/foo/1234e12`, uses attoparsec
    doubleHandle d = get $ text $ LT.pack (show d) <> " foos"

    -- `/athan@foo.com`
    emailRoute = r_ ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o_
    emailHandle e = get $ text $ LT.pack (show e) <> " email"

    -- `/baz`, uses regex-compat
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
```

Please see the [Hackage Documentation](http://hackage.haskell.org/package/nested-routes)
for more information.

## Installation

```bash
stack install nested-routes
```

## Contributing

Contact, fork, pull request, repeat :)

