![Logo](https://raw.githubusercontent.com/athanclark/nested-routes/master/logo.png)

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

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Web.Routes.Nested
import Data.Attoparsec.Text
import Text.Regex
import Data.Monoid
import qualified Data.Text.Lazy as LT
import Control.Monad.Error.Class
import Control.Monad.IO.Class

import Debug.Trace


data AuthRole = AuthRole deriving (Show, Eq)
data AuthErr = NeedsAuth deriving (Show, Eq)

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
  let app = routeAuth authorize routes
      routes =
        handle o (Just rootHandle) $ Just $ do
          handle fooRoute (Just fooHandle) $ Just $ do
            auth AuthRole unauthHandle ProtectChildren
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
    bazHandle = do
      get $ text "baz!"
      let uploader req = do liftIO $ print =<< strictRequestBody req
                            return $ Just ()
          uploadHandle Nothing = text "Upload Failed"
          uploadHandle (Just ()) = text "Woah! Upload content!"
      post uploader uploadHandle

    unauthHandle NeedsAuth = get $ textStatus status401 "Unauthorized!"
    notFoundHandle = get $ textStatus status404 "Not Found :("
```

Please see the [Hackage Documentation](http://hackage.haskell.org/package/nested-routes)
for more information.

## Installation

```bash
cabal install nested-routes
```

## Contributing

Fork, pull request, contact, repeat :)
