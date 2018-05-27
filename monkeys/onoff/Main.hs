{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import qualified LWWSet as L
import System.Random (randomRIO, randomIO)
import Control.Monad (forever, replicateM_)
import Control.Concurrent (threadDelay)
import System.Console.CmdArgs
import Data.Aeson (Value(Null), toJSON)
import Control.Concurrent.STM
import Data.Time.Clock.System
import Control.Lens

data Config = Config 
  { address :: String 
  , offlineOps :: Int
  , setName :: String
  } deriving (Show, Data, Typeable)

config :: Config 
config = Config 
  { address = "localhost:3000"
  , offlineOps = 100
  , setName = "default"
  }

getCurrentTimestamp :: IO L.TimeStamp
getCurrentTimestamp = do
    t <- getSystemTime
    return $ (toInteger (systemSeconds t) * 1000) + (toInteger ((systemNanoseconds t) `quot` 1000000))

main = do
    c <- cmdArgs config
    local <- newTVarIO (L.empty :: L.LWWSet String)
    forever $ do
        threadDelay $ 1000000
        let url = "http://" ++ (address c) ++ "/" ++ (setName c)
        replicateM_ (offlineOps c) $ do
            threadDelay 1000
            elem <- randomRIO (0, 100 :: Int)
            rem <- randomIO
            ts <- getCurrentTimestamp
            atomically $ do
                modifyTVar' local (\current ->
                  if rem then L.remove current (show elem) ts
                         else L.insert current (show elem) ts)
        current <- readTVarIO local
        post url (toJSON current)
        response <- asJSON =<< get url
        let fromServer = response ^. responseBody
        let merged = L.merge current fromServer
        if (merged == fromServer) then putStrLn "Success" else putStrLn "Failure"
        atomically $ do
            writeTVar local merged
