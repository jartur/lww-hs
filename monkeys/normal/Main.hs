{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import qualified LWWSet as L
import System.Random (randomRIO, randomIO)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import System.Console.CmdArgs
import Data.Aeson (Value(Null))

data Config = Config 
  { address :: String 
  , lowDelayMs :: Int
  , highDelayMs :: Int
  , setName :: String
  } deriving (Show, Data, Typeable)

config :: Config 
config = Config 
  { address = "localhost:3000"
  , lowDelayMs = 100
  , highDelayMs = 1000
  , setName = "default"
  }

main = do
    c <- cmdArgs config
    forever $ do
        delay <- randomRIO ((lowDelayMs c), (highDelayMs c))
        threadDelay $ delay * 1000
        read <- randomIO
        elem <- randomRIO (0, 100 :: Int)
        let url = "http://" ++ (address c) ++ "/" ++ (setName c) ++ "/" ++ (show elem)
        if read then 
            get url >>= putStrLn . show
         else do
            rem <- randomIO
            if rem then
                delete url >>= putStrLn . show
             else
                put url Null >>= putStrLn . show
