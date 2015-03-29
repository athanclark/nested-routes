{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Web.Routes.Nested where

import           Web.Routes.Nested.Internal

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.AddHeaders

import Data.Monoid
import Control.Applicative


data ArrowTrie t r = (t, Maybe r, Maybe r) :@-> [ArrowTrie t r]
  deriving (Show, Eq, Functor)

-- instance (Eq t, Monoid t) => Monoid (Arrow t r) where
--   mempty = (mempty, Nothing, Nothing) :@-> []
--   ((pss,xrest,xcurrent) :@-> xs) `mempty` ((qss,yrest,ycurrent) :@-> ys)
--     | pss == qss = (qss,yrest,ycurrent)

rest :: ArrowTrie t r -> Maybe r
rest ((_,r,_) :@-> _) = r

current :: ArrowTrie t r -> Maybe r
current ((_,_,r) :@-> _) = r

pathChunk :: ArrowTrie t r -> t
pathChunk ((t,_,_) :@-> _) = t

findWithPath :: Eq t => [ArrowTrie t r] -> t -> Maybe (ArrowTrie t r)
findWithPath [] _ = Nothing
findWithPath ((pss@(p,_,_) :@-> zs):xs) t | t == p = Just $ pss :@-> zs
                                          | otherwise = findWithPath xs t

applyWhenDef :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
applyWhenDef p f a [] = [a]
applyWhenDef p f a (x:xs) | p x = (f x) : xs
                          | otherwise = x : applyWhenDef p f a xs

-- | Eliminate children from a rose tree, and clear the indexed node's contents.
clear :: (Eq t, Monoid t) => ArrowTrie t r -> [t] -> ArrowTrie t r
clear trie [] = trie
clear (pss@(p,xrest,xcurrent) :@-> xs) tss@(t:ts)
  | p == mempty = pss :@-> map (flip clear tss) xs
  | p == t = case ts of
               -- found
               [] -> (p,Nothing,Nothing) :@-> []
               _  -> pss :@-> map (flip clear ts) xs
  | otherwise = pss :@-> xs


singleton :: Monoid t => [t] -> Maybe r -> ArrowTrie t r
singleton _      Nothing =  (mempty,Nothing,Nothing) :@-> []
singleton (t:[]) mcurrent = (t,Nothing,mcurrent)     :@-> []
singleton (t:ts) mcurrent = (t,mcurrent,Nothing)     :@-> [singleton ts mcurrent]

set :: (Eq t, Monoid t) => ArrowTrie t r -> [t] -> Maybe r -> ArrowTrie t r
set trie tss Nothing = clear trie tss
-- root node
set ((p,xrest,xcurrent) :@-> xs) (t:[]) mcurrent | t == mempty =
  (p,xrest,mcurrent) :@-> xs
-- can't set fingers with root, shouldn't happen
set (pss@(p,xrest,xcurrent) :@-> xs) [] _ = pss :@-> xs
set (pss@(p,xrest,xcurrent) :@-> xs) tss@(t:ts) mcurrent
  | p == mempty = pss :@-> map (\trie -> set trie tss mcurrent) xs
  | p == t = case ts of
      -- found
      []       -> (t,xrest,mcurrent) :@-> xs
      (t':ts') -> case xrest of
        -- _had_ unique decendant
        Just _ -> (p,Nothing,xcurrent) :@->
                    map (\trie -> set trie ts mcurrent) xs
        Nothing -> case xs of
          -- leaf
          []       -> (p,mcurrent,xcurrent) :@->
                        [singleton ts mcurrent]
          (x':xs') -> (p,Nothing,xcurrent) :@->
                        applyWhenDef
                          (\z -> pathChunk z == t')
                          (\trie -> set trie ts mcurrent)
                          (singleton ts mcurrent)
                          xs
  | otherwise = pss :@-> xs

-- import           Data.Aeson                        (ToJSON, encode)
-- import qualified Data.ByteString                   as B
-- import           Data.ByteString.Builder           (Builder)
-- import qualified Data.ByteString.Lazy              as LB
-- import qualified Data.Text                         as T
-- import qualified Data.Text.Encoding                as TR
-- import qualified Data.Text.Lazy                    as LT
-- import qualified Data.Text.Lazy.Encoding           as LTR

-- import qualified Data.Lucid                        as L
-- import qualified Text.Blaze.Html                   as H
-- import qualified Text.Blaze.Html.Renderer.Utf8     as HR

{-
Total ordering of HTTP Verbs, representing likelyness These will be the keys
in the Map:

GET:    0
POST:   1
PUT:    2
DELETE: 3
-}


-- TODO: Turn Actions / Responses into indicies for a writer monad?

-- | One for each major response type - html, json, and plaintext.
-- TODO: support binary with websockets?
-- type Responses = (Maybe Response, Maybe Response, Maybe Response)

-- type Action = (Method, Responses)

-- | One action for each (major) HTTP verb.
-- type Actions = (Maybe Action, Maybe Action, Maybe Action, Maybe Action)
--
-- data Handler = Handler { path :: Path
--                        , actions :: Actions
--                        , handlers :: [Handler]}


-- K, so, you need to parse the strings to see which data type is requested
-- (json, et al), then modify the responses so that the returned response has
-- a content-type header set to the corrosponding content type.

-- Also, in each request, we can look to see if there's an "accepts" header
-- in the request. Then, during the response, you can override..?

-- Each WAI request has a `requestMethod` function that I can use to see which
-- method was used.



-- newtype ActionM a = ActionM {runAction :: Reader (ResponseM ()) a}
--
-- newtype ResponseM a = ResponseM {runAction :: Writer }
--
-- get :: String -> ActionM ()
-- get xs = (methodGet, xs)
--
-- json :: ToJSON a => a -> ResponseM a

--
-- put :: Responses -> Action
-- put xs = (methodPut, xs)
--
-- post :: Responses -> Action
-- post xs = (methodPost, xs)
--
-- delete :: Responses -> Action
-- delete xs = (methodDelete, xs)


-- blaze :: Html -> Response
-- blaze = HR.renderHtml
-- lucid :: Monad m => HtmlT m () -> Response
-- lucid = L.renderTextT
-- json :: ToJSON a => a -> Response
-- json = lazy-bytestring . encode
-- text :: T.Text -> Response
-- text = bytestring . TR.encodeUtf8
-- lazy-text :: LT.Text -> Response
-- lazy-text = lazy-bytestring . LTR.encodeUtf8
-- builder :: Builder -> Response
-- builder = responseBuilder
-- bytestring :: B.ByteString -> Response
-- bytestring = lazy-bytestring . LB.fromStrict
-- lazy-bytestring :: LB.ByteString -> Response
-- lazy-bytestring = responseLBS


-- handle :: Path -> Actions -> [Handler] -> Handler
-- handle = Handler




-- | Prioritize GET requests and Html Urls first. For custom search priority,
-- manually manipulate the resulting list by applying some kind of "search and
-- move" function for lists.
-- collapse :: Handler -> [(String,Response)]
-- collapse hs = foldl1 go hs
--   where
--     go :: [(Int,Int,String,Response)]
--     go []
