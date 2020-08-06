module Server where

import           Prelude hiding (log)
import qualified Data.ByteString.Char8 as ByteString
import qualified Control.Exception as Exception
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Network.Simple.TCP (Socket, SockAddr)
import qualified Network.Simple.TCP as Network

import Logic.Update
import Types.Action
import Types.State
import Utils

initialState = State 0

runServer :: IO ()
runServer = do
  log "Starting Server"
  stateMVar <- MVar.newMVar initialState
  Network.serve (Network.Host "localhost") "8000" $ \(connectionSocket, remoteAddr) -> do
    log $ "TCP connection established from " ++ show remoteAddr ++ "."
    Exception.finally
      (process connectionSocket stateMVar)
      (log $ show remoteAddr ++ " disconnected.")

process :: Socket -> MVar State -> IO ()
process socket stateMVar =
  listen socket >>= maybe disconnected (\message -> do
    messageReceived socket stateMVar message
    process socket stateMVar
  )

disconnected :: IO ()
disconnected = log "Got empty message"

messageReceived :: Socket -> MVar State -> String -> IO ()
messageReceived socket stateMVar message = do
  response <- case safeRead message of
    Just action -> do
      state <- MVar.takeMVar stateMVar
      let state' = update action state
      MVar.putMVar stateMVar state'
      return $ show state'
    Nothing -> return "Unrecognized"
  send socket response

listen :: Socket -> IO (Maybe String)
listen = fmap (fmap ByteString.unpack) . flip Network.recv 1024

send :: Socket -> String -> IO ()
send sock = Network.send sock . ByteString.pack

log :: String -> IO ()
log = putStrLn
