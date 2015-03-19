nested-routes
=============

> Yet-Another WAI Router.

Declare your respondable locations / URI's in a composable way, much like a bi
rose tree, where closed nodes are HTTP methods and Content-Types _or_ open
nodes - nesting down. For example:

```haskell
handle ("foo" </> "bar")
  [ get [ html $ renderPage
        , json $ renderJson ]
  , post [ html $ renderAnotherPage
         , json $ renderAnotherThing ]
  ]
  [ handle ("baz") [get [html $ renderLastPage]] []]
```

Would handle `GET` and `POST` to `foo/bar`, `foo/bar.htm{,l}`, and
`foo/bar.json`, while `foo/bar/baz{,.htm{,l}}` only accepts `GET`.

Something like so:

```
route ::= [method * [response]] [route]

method ::= GET | POST | PUT | DELETE ...

response ::= HTML | JSON | TEXT ...
```

## Installation

TODO: Write installation instructions here

## Usage

TODO: Write usage instructions here

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
