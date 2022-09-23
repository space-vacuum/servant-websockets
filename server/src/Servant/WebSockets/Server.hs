{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.WebSockets.Server
 ( module Servant.WebSockets.Server
 , module Servant.WebSockets.API
 ) where

import Control.Monad (void)
import Data.Proxy (Proxy)
import Servant.Server
  ( ServerError(..), Context, HasServer, Server, ServerT, hoistServerWithContext, route, runHandler
  )

import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Servant.Server.Internal.Delayed (Delayed, runDelayed)
import Servant.Server.Internal.RouteResult (RouteResult(Fail, FailFatal, Route))
import Servant.Server.Internal.Router (Router, leafRouter)
import Servant.WebSockets.API (WebSocketApp)

import qualified Network.WebSockets as WS
import Data.Kind (Type)

instance HasServer WebSocketApp context where
  type ServerT WebSocketApp m = WebSocketServerT m
  hoistServerWithContext _api = hoistWebSocketServerWithContext
  route _api = routeWebSocket

type WebSocketServerT m = WS.PendingConnection -> m ()

hoistWebSocketServerWithContext
  :: Proxy (context :: [Type])
  -> (forall x. m x -> n x)
  -> ServerT WebSocketApp m
  -> ServerT WebSocketApp n
hoistWebSocketServerWithContext _ = (.)

-- Adapted from https://github.com/moesenle/servant-websockets/blob/6ff9f0eeda0e0c228e82bae54f729115a33a3925/src/Servant/API/WebSocket.hs#L90-L104
routeWebSocket
  :: Context context
  -> Delayed env (Server WebSocketApp)
  -> Router env
routeWebSocket _ app =
  leafRouter $ \env request respond ->
    runResourceT $
      runDelayed app env request
        >>= liftIO . go request respond
  where
  go request respond = \case
    Fail e -> respond $ Fail e
    FailFatal e -> respond $ FailFatal e
    Route app' ->
      websocketsOr
        WS.defaultConnectionOptions
        (runApp app')
        (backupApp respond)
        request
        (respond . Route)

  runApp a c =
    void $ runHandler $
      a c
      -- TODO: Likely not right, but fixes the tests.
      `catch` (\case
        WS.ConnectionClosed -> pure ()
        e -> throwM e)

  backupApp respond _ _ =
    respond $ Fail ServerError
      { errHTTPCode = 426
      , errReasonPhrase = "Upgrade Required"
      , errBody = mempty
      , errHeaders = mempty
      }
