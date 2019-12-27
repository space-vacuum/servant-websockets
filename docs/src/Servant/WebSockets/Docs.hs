module Servant.WebSockets.Docs where

import Control.Lens ((&), (.~), (^.))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Network.HTTP.Media (MediaType)
import Servant.API.ContentTypes (AllMimeRender)
import Servant.Docs
  ( HasDocs(docsFor), ToSample, maxSamples, method, respBody, respStatus, response, rqbody
  , sampleByteStrings, single
  )

import qualified Data.ByteString.Lazy as Lazy

-- | Derive docs from the given 'req' and 'resp' types. Each should
-- either be '()' for none (meaning the websocket either does not receive or
-- send messages) or '('[ct], a)' for content-type samples.
newtype DeriveWebSocketDocs (req :: reqK) (resp :: respK) api =
  DeriveWebSocketDocs api

class WebSocketSamples (a :: k) where
  webSocketSamples :: Proxy a -> [(Text, MediaType, Lazy.ByteString)]

instance WebSocketSamples () where
  webSocketSamples _ = []

instance
  (AllMimeRender (ct ': cts) a, ToSample a)
  => WebSocketSamples '((ct ': cts), a)
  where
  webSocketSamples _ = sampleByteStrings (Proxy @(ct ': cts)) (Proxy @a)

instance
  (WebSocketSamples req, WebSocketSamples resp)
  => HasDocs (DeriveWebSocketDocs req resp api)
  where
  docsFor _ (endpoint, action) docOpts = single endpoint' action'
    where
    -- Assume the standard close status code 1000
    -- See https://tools.ietf.org/html/rfc6455#section-7.4
    normalCloseStatus = 1000

    takeSamples = take (docOpts ^. maxSamples)

    endpoint' = endpoint & method .~ "WEBSOCKET"

    action' =
      action
        & rqbody .~ takeSamples (webSocketSamples (Proxy @req))
        & response . respStatus .~ normalCloseStatus
        & response . respBody .~ takeSamples (webSocketSamples (Proxy @resp))
