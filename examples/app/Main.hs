module Main where

import Control.Monad
import Data.Maybe

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Servant.Examples.WebSockets.MyExample (chat, mkMyClient, newMyWaiApp)
import System.Environment (getArgs, lookupEnv)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  port <- maybe 9160 read <$> lookupEnv "WS_PORT"
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "WS_HOST"
  getArgs >>= \case
    ["server"] -> Warp.run port =<< newMyWaiApp
    ["client", user] -> clientApp host port user
    args -> error $ "Invalid arguments" <> show args
  where
  clientApp host port user = do
    httpManager <- HTTPClient.newManager HTTPClient.defaultManagerSettings
    chat (mkMyClient httpManager port) (fromString user) $ \conn -> do
      putStrLn "Connected!"
      -- Fork a thread that writes WS data to stdout
      _ <- forkIO $ forever $ do
          msg <- WS.receiveData conn
          liftIO $ Text.putStrLn msg
      -- Read from stdin and write to WS
      let loop = do
              line <- Text.getLine
              unless (Text.null line) $ WS.sendTextData conn line >> loop
      loop
      WS.sendClose conn (Text.pack "Bye!")
