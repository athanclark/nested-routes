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
      json ("wheeeee" :: T.Text))
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


## Installation

```bash
cabal install nested-routes
```

## Contributing

Fork, pull request, contact, repeat :)
