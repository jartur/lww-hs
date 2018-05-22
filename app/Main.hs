module Main where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" (\p -> ("127.0.0.1", p)) defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        -- get our own process id
        self <- getSelfPid
        send self "whello"
        hello <- expect :: Process String
        liftIO $ putStrLn hello
