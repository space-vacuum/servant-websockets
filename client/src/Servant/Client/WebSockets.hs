{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Client.WebSockets where

import Data.Foldable
import Data.Proxy
import Servant.Client
import Servant.Client.Core

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask))
import Data.Binary.Builder (toLazyByteString)
import Network.Socket (withSocketsDo)
import Servant.API.WebSockets (WebSocketApp)

import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Network.WebSockets as WS

instance
  ( MonadReader ClientEnv m
  , MonadIO m
  , RunClient m
  ) => HasClient m WebSocketApp
  where
  type Client m WebSocketApp = WebSocketClient m
  hoistClientMonad = hoistWebSocketClientMonad
  clientWithRoute = webSocketClientWithRoute

type WebSocketClient m = WS.ClientApp () -> m ()

hoistWebSocketClientMonad
  :: ( Client mon api ~ WebSocketClient mon
     , Client mon' api ~ WebSocketClient mon'
     )
  => Proxy (m :: * -> *)
  -> Proxy api
  -> (forall x. mon x -> mon' x)
  -> Client mon api
  -> Client mon' api
hoistWebSocketClientMonad _ _ = (.)

webSocketClientWithRoute
  :: ( MonadIO m
     , MonadReader ClientEnv m
     , Client m api ~ WebSocketClient m
     )
  => Proxy (m :: * -> *)
  -> Proxy api
  -> Request
  -> Client m api
webSocketClientWithRoute _ _ req wsApp = do
  let headers = toList $ requestHeaders req
  let path = LazyChar8.unpack $ toLazyByteString $ requestPath req
  clientEnv <- ask
  liftIO $ withSocketsDo $
    WS.runClientWith
      (baseUrlHost $ baseUrl clientEnv)
      (baseUrlPort $ baseUrl clientEnv)
      path
      WS.defaultConnectionOptions
      headers
      wsApp