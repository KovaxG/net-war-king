module Server where

import           Prelude hiding (log)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Exception as Exception
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Network.Simple.TCP (Socket, SockAddr)
import qualified Network.Simple.TCP as Network

import Logic.Update
import Types.Common
import Types.Action
import Types.Response
import Types.State
import Utils

runServer :: IO ()
runServer = do
  log "Starting Server"
  stateMVar <- MVar.newMVar initialState
  Network.serve (Network.Host "localhost") "8000" $ \(connectionSocket, remoteAddr) -> do
    let sessionID = show remoteAddr
    log $ "TCP connection established from " ++ sessionID ++ "."
    Exception.finally
      (process connectionSocket sessionID stateMVar)
      (do
        mutateState sessionID stateMVar Logout
        log $ sessionID ++ " disconnected."
      )

process :: Socket -> SessionID -> MVar State -> IO ()
process socket sessionID stateMVar =
  listen socket >>= maybe disconnected (\message -> do
    messageReceived socket sessionID stateMVar message
    process socket sessionID stateMVar
  )

disconnected :: IO ()
disconnected = log "Got empty message"

messageReceived :: Socket -> SessionID -> MVar State -> String -> IO ()
messageReceived socket sessionID stateMVar message =
  send socket =<< case actionParser message of
    Nothing -> return "Unrecognized"
    Just action -> show <$> mutateState sessionID stateMVar action

listen :: Socket -> IO (Maybe String)
listen = fmap (fmap ByteString.unpack) . flip Network.recv 1024

send :: Socket -> String -> IO ()
send sock = Network.send sock . ByteString.pack

log :: String -> IO ()
log = putStrLn

mutateState :: SessionID -> MVar State -> Action -> IO Response
mutateState sessionID stateMVar action = do
  state <- MVar.takeMVar stateMVar
  let (state', response) = update sessionID action state
  log $ show state'
  MVar.putMVar stateMVar state'
  return response
