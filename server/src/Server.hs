module Server where

import qualified Control.Exception as Exception
import           Network.Simple.TCP (Socket, SockAddr)
import qualified Network.Simple.TCP as Network

import Logic.Update
import Logic.Tick

import Types.Action
import Types.State

runServer :: IO ()
runServer = do
  putStrLn "Starting Server"
  Network.serve (Network.Host "localhost") "8000" $ \(connectionSocket, remoteAddr) ->
    Exception.finally
      (process connectionSocket remoteAddr)
      (putStrLn $ show remoteAddr ++ " disconnected.")

process :: Socket -> SockAddr -> IO ()
process socket addr = do
  putStrLn $ "TCP connection established from " ++ show addr ++ "."
