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
      handleLit o
        (Left $ get $ text "home")
        Nothing
      handleLit (l "foo" </> l "bar" </> o)
        (Left $ get $ text "foobar") $ Just $
        handleParse (p ("baz",double) </> o)
          (\d -> Right $ get $ textOnly $ LT.pack (show d) `LT.append` " bazs")
          Nothing
      handleParse (p ("num",double) </> o)
        (\d -> Right $ get $ textOnly $ LT.pack $ show d) $ Just $
        handleLit (l "bar" </> o)
           (\d -> Left $ get $ text $ (LT.pack $ show d) `LT.append` " bars")
           Nothing
```

The route specification syntax is a little strange right now - `l` specifies                                          
a "literal chunk" of a handlable url (ie - `l "foo" </> l "bar" </> o` would                                                 
represent the url `/foo/bar`), while `p` represents a "parsable" url chunk,                                                                     
which expects a pair - the left element being merely a reference name for the                                                               
parser during internal plumbing, and the right being the actual `Parser`. `o` represents
the end of a url string, and can be used alone in a handler to capture requests
to the root path.
.
Each route being handled needs some kind of content - that's where the `Either`
stuff comes in to play. For every parsed url chunk, the route expects a function
of arity matching 1-for-1 with the parsed contents. For example, `\d -> ...` in the
demonstration above is such a function, where `d :: Double`.
.
We use the `Either` for a subtle reason - literal url strings may have a file
extension, while url strings ending with a parser would not. `get`, `post`, etc.
are all monadic expressions, accumulating a `Map` for HTTP verbs, likewise with
`text`, `lucid`, `json`, `bytestring` etc., where they may also match a particular
file extension. `textOnly` and the other `-Only` variants are not monadic, and
simply give us a convenient unwrapper. Basically, url paths ending with a literal
chunk are `Left` and contain a `VerbListenerT z (FileExtListenerT Response m ()) m ()`,
while paths ending with a parser are `Right` and contain `VerbListenerT z Response m ()`.

When we test our application:

```bash
λ> curl localhost:3000/
↪ "home"
```

requests may end with index

```bash
λ> curl localhost:3000/index
↪ "home"
```

and specify the file extension

```bash
λ> curl localhost:3000/index.txt
↪ "home"
```

each responding with the "closest" available file type

```bash
λ> curl localhost:3000/index.html
↪ "home"
```

```bash
λ> curl localhost:3000/foo/bar
↪ "foobar"
```

```bash
λ> curl localhost:3000/foo/bar.txt
↪ "foobar"
```

```bash
λ> curl localhost:3000/foo/bar/5678.5678
↪ "5678.5678 bazs"
```

```bash
λ> curl localhost:3000/1234.1234
↪ "1234.1234"
```

```bash
λ> curl localhost:3000/2e5
↪ "200000.0"
```

```bash
λ> curl localhost:3000/1234.1234/bar
↪ "1234.1234 bars"
```

## Installation

```bash
cabal install nested-routes
```

## Contributing

Fork, pull request, contact, repeat :)
