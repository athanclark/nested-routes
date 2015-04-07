nested-routes
=============

> Yet-Another WAI Router.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Web.Routes.Nested
import qualified Data.Text as T

router :: Application
router = route $ do
  handle [] -- root
    (get $ do
      text "woo!"
    [ handle ["wat","the","heck"]
        (get $
          text "idk man, I'm just here for the beer")
        []
    , handle ["meh"]
        (get $
          text "I dunno")
        []
    ]
  handle ["foo"]
    (get $ text "foo")
    [ handle ["bar"]
        (get $ do
          text "bar-ography"
          json ("{book: \"barography\"}" :: T.Text))
        []
    ]
```

```bash
curl localhost:3000/

↪ woo!

curl localhost:3000/foo

↪ foo

curl localhost:3000/foo/bar

↪ bar-orgraphy

curl localhost:3000/foo/bar.json

↪ {book: "barography"}
```

## Installation

```bash
cabal install nested-routes
```

## Contributing

Fork, pull request, contact, repeat :)
