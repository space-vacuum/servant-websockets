module ServantWebSocketsSpec where

import Control.Monad
import Data.Proxy
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic
import Servant.Server
import Servant.Server.Generic
import Test.Hspec

import Control.Concurrent
  ( MVar, forkIO, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar
  , threadDelay
  )
import Control.Concurrent.Async (race)
import Control.Exception (finally, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import Servant.API.WebSockets (WebSocketApp)
import Servant.Client.WebSockets ()
import Servant.Server.WebSockets ()
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

spec :: Spec
spec = do
  describe "MyAPI" $ do
    it "should work" $ withTestClient $ \myClient -> do
      jan <- newUser
      void $ forkIO $ foo myClient "Jan" $ wsIO jan
      receive jan `shouldReturn` "Welcome!"
      send jan "Hi!"
      receive jan `shouldReturn` "Jan: Hi!"

      tim <- newUser
      void $ forkIO $ foo myClient "Tim" $ wsIO tim

      receive tim `shouldReturn` "Welcome! Users: Jan"
      receive jan `shouldReturn` "Tim joined"

      send tim "Yo"
      receive tim `shouldReturn` "Tim: Yo"
      receive jan `shouldReturn` "Tim: Yo"

      kill tim
      receive jan `shouldReturn` "Tim disconnected"
      where
      withTimeout :: (HasCallStack) => IO a -> IO a
      withTimeout x = do
        -- 10 seconds
        race (threadDelay 10000000) x >>= \case
          Left () -> error "Failed to complete within timeout"
          Right a -> pure a

      -- (connection var, die var)
      newUser :: IO (MVar WS.Connection, MVar ())
      newUser = (,) <$> newEmptyMVar @WS.Connection <*> newEmptyMVar @()

      send :: (HasCallStack) => (MVar WS.Connection, MVar ()) -> Text -> IO ()
      send (var, _) msg =
        withTimeout $ readMVar var >>= flip WS.sendTextData msg

      receive :: (HasCallStack) => (MVar WS.Connection, MVar ()) -> IO Text
      receive (var, _) = withTimeout $ readMVar var >>= WS.receiveData

      kill :: (HasCallStack) => (MVar WS.Connection, MVar ()) -> IO ()
      kill (_, die) = withTimeout $ void $ putMVar die ()

      wsIO :: (MVar WS.Connection, MVar ()) -> WS.Connection -> IO ()
      wsIO (var, die) conn = do
        putMVar var conn -- Set the connection var
        takeMVar die -- Wait until the die var is set
        void $ takeMVar var -- Unset the connection var

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
