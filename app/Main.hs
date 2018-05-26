{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import qualified LWWSet as L
import Web.Scotty  
import Data.Monoid (mconcat)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import qualified Control.Concurrent.STM as STM

type StringSet = L.LWWSet String
ss :: L.LWWSet String
ss = L.remove (L.unit "setty" 1) "boobies" 2

main = scotty 3000 $ do
    get "/:set" $ do
       -- setName <- param "set"
        json $ ss
    get "/:set/:elem" $ do
       -- setName <- param "set"
        elem <- param "elem"
        json $ L.query ss elem

data AppState = AppState 
  { appSet :: STM.TVar StringSet
  , toReplicate :: STM.TVar StringSet
  }

newtype AppStateM a = AppStateM 
  { runAppStateM :: ReaderT AppState IO a

  }