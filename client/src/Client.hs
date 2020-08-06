module Client (someFunc) where

import qualified Data.ByteString.Char8 as ByteString
import           Network.Simple.TCP (Socket)
import qualified Network.Simple.TCP as Network
import qualified System.CPUTime as Time

someFunc :: IO ()
someFunc = do
    putStrLn "Starting Client"
    Network.connect "localhost" "8000" $ \(socket, remoteAddr) -> do
        putStrLn $ "Connection established to " ++ show remoteAddr
        loop socket

loop :: Socket -> IO ()
loop socket = do
    putStr "> "
    message <- getLine
    t0 <- Time.getCPUTime
    Network.send socket (ByteString.pack message)
    putStr "| "
    response <- Network.recv socket 1024
    t1 <- Time.getCPUTime
    case response of
        Just r -> do
            putStr (ByteString.unpack r)
            putStrLn "."
        Nothing -> putStrLn "NO MESSAGE."
    putStrLn $ "dt = " ++ show ((t1 - t0) `div` 1000000) ++ " ms"
    loop socket
