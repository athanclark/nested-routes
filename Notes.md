Notes
=====

From what I see, a lot of these authentication middlewares try to take control
over the routing at an earlier layer in the pipeline. If we want to have
seams of authenticated access with nested-routes, we would first need our own
combinator for:

- conditional rerouting
    - _two_ responses for immediate node - failure remains default for children
      routes, much like `notFound`.
- how to authenticate
    - header lookup - easy, always have access
    - hashing - ought to just embed main monad as accessible (is MonadIO).

It would not be wise to do inter-middleware inter-op with nested-routes, I think:

- The internal structure is a pretty-deeply existentially quantified rose tree:

```haskell
data PredTrie t a =
              More  t             (Maybe a)        [PredTrie t a]
  | forall r. Pred (t -> Maybe r) (Maybe (r -> a)) [PredTrie t (r -> a)]
```

That is to say, the contents and children of any `Pred` constructor are incompatible
with any other cons cell. I use a Writer monad for each layer of depth being
constructed, where adjacent binding is the monoidal `<>`.

This becomes more difficult, too, as `UrlChunks` is basically an `HList`, where
each new type found while recursing down the string of chunks _needs_ to be
handled completely, _immediately_ - a depth-first construction (for children monadic
statements).

## Critical Needs

- Internal redirection
