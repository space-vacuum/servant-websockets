module ServantWebSocketsSpec where

import Control.Monad
import Test.Hspec

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (race)
import Data.Text (Text)
import Servant.WebSockets.Examples.MyExample (MyRoutes(chat), MyClient, mkMyClient, newMyWaiApp)
import System.IO.Unsafe (unsafePerformIO)

import qualified Network.HTTP.Client as HTTPClient
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

spec :: Spec
spec =
  around withTestClient $ do
    describe "MyAPI" $ do
      describe "/chat" $ do
        it "should work" $ \myClient -> do
          let withTimeout :: (HasCallStack) => IO a -> IO a
              withTimeout x = do
                -- 10 seconds
                race (threadDelay 10000000) x >>= \case
                  Left () -> error "Failed to complete within timeout"
                  Right a -> pure a

          -- (connection var, die var)
          let newUser :: IO (MVar WS.Connection, MVar ())
              newUser = (,) <$> newEmptyMVar @WS.Connection <*> newEmptyMVar @()

          let send :: (HasCallStack) => (MVar WS.Connection, MVar ()) -> Text -> IO ()
              send (var, _) msg =
                withTimeout $ readMVar var >>= flip WS.sendTextData msg

          let receive :: (HasCallStack) => (MVar WS.Connection, MVar ()) -> IO Text
              receive (var, _) = withTimeout $ readMVar var >>= WS.receiveData

          let kill :: (HasCallStack) => (MVar WS.Connection, MVar ()) -> IO ()
              kill (_, die) = withTimeout $ void $ putMVar die ()

          let wsIO :: (MVar WS.Connection, MVar ()) -> WS.Connection -> IO ()
              wsIO (var, die) conn = do
                putMVar var conn -- Set the connection var
                takeMVar die -- Wait until the die var is set
                void $ takeMVar var -- Unset the connection var

          jan <- newUser
          void $ forkIO $ chat myClient "Jan" $ wsIO jan
          receive jan `shouldReturn` "Welcome!"
          send jan "Hi!"
          receive jan `shouldReturn` "Jan: Hi!"

          tim <- newUser
          void $ forkIO $ chat myClient "Tim" $ wsIO tim

          receive tim `shouldReturn` "Welcome! Users: Jan"
          receive jan `shouldReturn` "Tim joined"

          send tim "Yo"
          receive tim `shouldReturn` "Tim: Yo"
          receive jan `shouldReturn` "Tim: Yo"

          kill tim
          receive jan `shouldReturn` "Tim disconnected"

        -- TODO: This seems to be thrown by the server in the following test -
        -- MalformedResponse (ResponseHead {responseCode = 403, responseMessage = "Forbidden", responseHeaders = []}) "Wrong response status or message."
        --
        -- it "should reject Bill" $ \myClient -> do
        --   let go =
        --         chat myClient "Bill" $ \_conn -> do
        --           expectationFailure "This block should not execute"

        --   try @ClientError go >>= \case
        --     Right () -> expectationFailure "Expected Left, got Right "
        --     Left (FailureResponse _ r) -> expectationFailure $ "********** FailureResponse " <> show r
        --     Left e -> throwIO e


withTestClient :: (HasCallStack) => (MyClient -> IO a) -> IO a
withTestClient withClient = do
  Warp.testWithApplicationSettings Warp.defaultSettings newMyWaiApp $ \port ->
    withClient $ mkMyClient globalHTTPManager port

{-# NOINLINE globalHTTPManager #-}
globalHTTPManager :: HTTPClient.Manager
globalHTTPManager =
  unsafePerformIO $ HTTPClient.newManager HTTPClient.defaultManagerSettings
