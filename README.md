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
router :: Application
router = route handlers
  where
    handlers = do
      handle o
        (get $ text "home")
        Nothing
      handle ("foo" </> "bar")
        (get $ text "foobar") $ Just $
        handle (p ("baz", double) </> o)
          (\d -> get $ text $ LT.pack (show d) <> " bazs")
          Nothing
      handle (p ("num",double) </> o)
        (\d -> get $ text $ LT.pack $ show d) $ Just $ do
        handle "bar"
           (\d -> get $ do
                    text $ (LT.pack $ show d) <> " bars")
                    json $ (LT.pack $ show d) <> " bars!")
           Nothing
        handle (r ("email", mkRegex "(^[-a-zA-Z0-9_.]+@[-a-zA-Z0-9]+\\.[-a-zA-Z0-9.]+$)") </> o)
           (\d e -> get $ textOnly $ (LT.pack $ show d) <> " " <> (LT.pack $ show e)
```

Please see the [Hackage Documentation](http://hackage.haskell.org/package/nested-routes)
for more information.

## Installation

```bash
cabal install nested-routes
```

## Contributing

Fork, pull request, contact, repeat :)
