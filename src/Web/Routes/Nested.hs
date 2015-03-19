module Web.Routes.Nested where

import           Web.Routes.Nested.Internal

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.AddHeaders

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


type Path = [String]

(</>) :: String -> Path -> Path
(</>) = (:)

infixr 9 </>

-- | One for each major response type - html, json, and plaintext.
-- TODO: support binary with websockets?
type Responses = (Maybe Response, Maybe Response, Maybe Response)

type Action = (Method, Responses)

-- | One action for each (major) HTTP verb.
type Actions = (Maybe Action, Maybe Action, Maybe Action, Maybe Action)

data Handler = Handler { path :: Path
                       , actions :: Actions
                       , handlers :: [Handler]}


get :: Responses -> Action
get xs = (methodGet, xs)

put :: Responses -> Action
put xs = (methodPut, xs)

post :: Responses -> Action
post xs = (methodPost, xs)

delete :: Responses -> Action
delete xs = (methodDelete, xs)


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


handle :: Path -> Actions -> [Handler] -> Handler
handle = Handler
