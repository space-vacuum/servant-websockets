module ServantWebSocketsSpec where

import Control.Monad
import Data.Proxy
import Servant.API
import Servant.API.Generic
import Servant.API.WebSockets
import Servant.Client
import Servant.Client.Generic
import Servant.Client.WebSockets ()
import Servant.Server
import Servant.Server.Generic
import Servant.Server.WebSockets ()
import Test.Hspec

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

spec :: Spec
spec = do
  describe "MyAPI" $ do
    it "should work" $ withTestClient $ \myClient -> do
      let runTim = foo myClient "Tim" $ \conn -> do
            WS.receiveData @Text conn `shouldReturn` "Welcome! Users: Jan"
            WS.sendTextData @Text conn "Yo"
            WS.receiveData @Text conn `shouldReturn` "Tim: Yo"

      foo myClient "Jan" $ \conn -> do
        WS.receiveData @Text conn `shouldReturn` "Welcome!"
        WS.sendTextData @Text conn "Hi!"
        WS.receiveData @Text conn `shouldReturn` "Jan: Hi!"
        runTim
        WS.receiveData @Text conn `shouldReturn` "Tim joined"
        WS.receiveData @Text conn `shouldReturn` "Tim: Yo"
        -- TODO: This fails with a ConnectionException (ConnectionClosed)
        -- Probably need to fork these into threads and write to them
        -- with TChan or something.
        WS.receiveData @Text conn `shouldReturn` "Tim disconnected"

type MyAPI = ToServantApi MyRoutes

myAPI :: Proxy MyAPI
myAPI = genericApi (Proxy @MyRoutes)

data MyRoutes routes = MyRoutes
  { foo :: routes :-
      Header' '[Required, Strict] "Authorization" User
        :> "foo"
        :> WebSocketApp
  } deriving stock (Generic)

type MyServer = MyRoutes AsServer

mkMyServer :: MVar MyServerState -> MyServer
mkMyServer state = MyRoutes { foo }
  where
  foo :: User -> WS.PendingConnection -> Handler ()
  foo user pendingConn = liftIO $ do
    s <- readMVar state
    if user == "Bill" then
      WS.rejectRequest pendingConn "No Bills allowed!"
    else if any ($ user) [Text.null, Text.any isPunctuation, Text.any isSpace] then
      WS.rejectRequest pendingConn "Name cannot contain punctuation or whitespace, and cannot be empty"
    else if any (user ==) (map fst s) then
      WS.rejectRequest pendingConn "User already exists"
    else
      start =<< WS.acceptRequest pendingConn
    where
    start conn = do
      WS.forkPingThread conn 30
      flip finally disconnect $ do
        s <- readMVar state
        WS.sendTextData conn $ case s of
          [] -> "Welcome!"
          _  -> "Welcome! Users: " <> Text.intercalate ", " (map fst s)
        broadcast $ user <> " joined"
        modifyMVar_ state $ pure . ((user, conn) :)
        talk conn

    disconnect = do
      modifyMVar_ state $ pure . filter ((user /=) . fst)
      broadcast (user <> " disconnected")

    broadcast message = do
      s <- readMVar state
      forM_ s $ \(_, conn) -> WS.sendTextData conn message

    talk conn = forever $ do
      message <- WS.receiveData conn
      broadcast $ user <> ": " <> message

type Greeting = Text
type User = Text
type MyServerState = [(User, WS.Connection)]

type MyClient = MyRoutes (AsClientT IO)

mkMyClient :: HTTPClient.Manager -> Int -> MyClient
mkMyClient httpManager port =
  genericClientHoist $ \x ->
    runClientM x clientEnv >>= either throwIO pure
  where
  clientEnv =
    mkClientEnv httpManager $
      BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = port
        , baseUrlPath = ""
        }

withTestClient :: (HasCallStack) => (MyClient -> IO a) -> IO a
withTestClient withClient = do
  state <- newMVar []
  withServer state $ \port ->
    withClient $ mkMyClient globalHTTPManager port
  where
  mkWaiApp state = genericServe $ mkMyServer state
  withServer state =
    Warp.testWithApplicationSettings Warp.defaultSettings (pure $ mkWaiApp state)

{-# NOINLINE globalHTTPManager #-}
globalHTTPManager :: HTTPClient.Manager
globalHTTPManager =
  unsafePerformIO $ HTTPClient.newManager HTTPClient.defaultManagerSettings
