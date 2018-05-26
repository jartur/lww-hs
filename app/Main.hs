{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified LWWSet as L
import SqlModel

import Web.Scotty.Trans (ActionT, ScottyT, Options, scottyT, defaultHandler, delete, get, json, jsonData, middleware,
  notFound, param, post, put, scottyOptsT, settings,
   showError, status, verbose)
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404)
import Data.Monoid (mconcat)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Concurrent.STM as STM
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.Text.Lazy as T
import Data.Text as TS
import Data.Aeson (Value (Null))
import qualified Data.Default as DD
import Data.Time.Clock.System
import qualified Data.Map.Strict as M

import qualified Database.Persist as DB
import qualified Database.Persist.Sqlite as DB

import System.Console.CmdArgs

type StringSet = L.LWWSet String
type Handler = ActionT T.Text AppStateM ()

{-
 read db on start
 update the peers list by merging it with command line
 port cmdline
-}
main = do 
  c <- cmdArgs config
  DB.runSqlite (dbFile c) $ do
   DB.runMigration migrateAll 
  s <- initAppState c
  let r m = runReaderT (runAppStateM m) s
  scottyT 3000 r app 

app :: ScottyT T.Text AppStateM ()
app = do
    get "/:set" getSetHandler
    get "/:set/:elem" getElemHandler
    put "/:set/:elem" putElementHandler
    delete "/:set/:elem" deleteElementHandler
    post "/:set" postSetHandler

putElementHandler :: Handler
putElementHandler = do 
    setName <- param "set"
    elem <- param "elem"
    s <- lift $ asks appSet  
    liftIO $ do
        t <- getSystemTime
        let ts = (toInteger (systemSeconds t) * 1000) + (toInteger ((systemNanoseconds t) `quot` 1000000))
        STM.atomically $ do 
            sets <- readTVar s
            let current = M.findWithDefault (L.empty) setName sets
            let updated = L.insert current elem ts
            writeTVar s $ M.insert setName updated sets
    json Null

deleteElementHandler :: Handler
deleteElementHandler = json Null

postSetHandler :: Handler
postSetHandler = json Null


-- runQuery (DB.get (SetModelKey setName))

getSetHandler :: Handler
getSetHandler = do
    setName <- param "set"
    s <- lift (asks appSet)
    ls <- liftIO $ do 
        sets <- readTVarIO s
        return $ M.lookup setName sets
    case ls of
        Nothing -> do
            status notFound404
            json Null
        Just res ->  
            json $ res

getElemHandler :: Handler
getElemHandler = do
     setName <- param "set"
     elem <- param "elem"
     ls <- runQuery (DB.get (SetModelKey setName))
     case ls of
        Nothing -> do
            status notFound404
            json Null
        Just res ->  
            json $ L.query (setModelSet res) elem

data AppState = AppState 
  { appSet :: STM.TVar (M.Map String StringSet)
  , toReplicate :: STM.TVar StringSet
  , dbPool :: DB.ConnectionPool
  }

newtype AppStateM a = AppStateM 
  { runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad,
    MonadIO, MonadReader AppState)

data Config = Config 
  { initialPeers :: [String] 
  , dbFile :: TS.Text
  } deriving (Show, Data, Typeable)

config :: Config 
config = Config 
  { initialPeers = []
  , dbFile = "sqlite3.db"    
  }

createDBPool :: Config -> IO DB.ConnectionPool
createDBPool c = do
    let connectionString = dbFile c
    let poolSize = 1
    runStdoutLoggingT (DB.createSqlitePool connectionString poolSize)

initAppState :: Config -> IO AppState
initAppState c = do
    p <- createDBPool c
    sv <- STM.newTVarIO $ (M.empty :: M.Map String StringSet) 
    rv <- STM.newTVarIO $ (L.empty :: StringSet)
    return AppState 
        { appSet = sv
        , toReplicate = rv
        , dbPool = p
        }

runQuery :: (MonadTrans t, MonadIO (t AppStateM)) => 
    DB.SqlPersistT IO a -> t AppStateM a
runQuery q = do
    p <- lift (asks dbPool)
    liftIO $ DB.runSqlPool q p