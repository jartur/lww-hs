{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import qualified LWWSet as L
import System.Random (randomRIO, randomIO)
import Control.Monad (forever, replicateM_)
import Control.Concurrent (threadDelay, forkIO)
import System.Console.CmdArgs
import Data.Aeson (Value(Null), toJSON)
import Control.Concurrent.STM
import Data.Time.Clock.System
import Control.Lens

data Config = Config 
  { address :: String 
  , updateIntervalMs :: Int
  , setName :: String
  } deriving (Show, Data, Typeable)

config :: Config 
config = Config 
  { address = "localhost:3000"
  , updateIntervalMs = 1000
  , setName = "default"
  }

main = do
    c <- cmdArgs config
    local <- newTVarIO (L.empty :: L.LWWSet String)
    forkIO $ forever $ do
        threadDelay $ ((updateIntervalMs c) `quot` 2) * 1000
        current <- readTVarIO local
        putStrLn $ show current
    forever $ do
        threadDelay $ (updateIntervalMs c) * 1000
        let url = "http://" ++ (address c) ++ "/" ++ (setName c)
        response <- asJSON =<< get url
        let fromServer = response ^. responseBody
        atomically $ do
            modifyTVar' local (\current -> L.merge current fromServer)
    