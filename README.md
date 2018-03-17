![Logo](https://cdn.rawgit.com/athanclark/nested-routes-website/master/static/images/logo.svg)

```haskell
routes :: RouterT (MiddlewareT m) sec m ()
routes = do
  matchHere $ action $ do
    get $ do
      json ("some cool json", True, 12) -- application/json
      text "Yo" -- text/plain
  matchGroup (l_ "someChunk" </> o_) $ do
    match (p_ "some parser" Attoparsec.doube </> o_) $ \(d :: Double) -> -- /someChunk/124.234 would match
      action $ ...
    matchGroup (r_ [regex|/^(\.)+(.*)/|] </> o_) $ \(matches :: [String]) -> -- /someChunk/....huh? would match
      action $ ...
      
      
myMiddleware :: MiddlewareT m
myMiddleware = route routes
```
