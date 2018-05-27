{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified LWWSet as L
import SqlModel

import Web.Scotty.Trans (ActionT, ScottyT, Options, scottyT, defaultHandler, delete, get, json, jsonData, middleware,
  notFound, param, post, put, scottyOptsT, settings, showError, status, verbose)
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404)
import Data.Monoid (mconcat)
import Control.Monad (when, forever, forM, forM_)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT, ask)
import Control.Concurrent.STM as STM
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Aeson (Value (Null), object, toJSON)
import qualified Data.Default as DD
import Data.Time.Clock.System
import qualified Data.Map.Strict as M
import Control.Concurrent (forkIO, threadDelay)
import qualified Network.Wreq as W

import qualified Database.Persist as DB
import qualified Database.Persist.Sqlite as DB

import System.Console.CmdArgs

type StringSet = L.LWWSet String
type Handler = ActionT T.Text AppStateM ()
type SubHandler a = ActionT T.Text AppStateM a

{-
 update the peers list by merging it with command line
-}
peerSetName :: String
peerSetName = "_peers_"

main = do 
  c <- cmdArgs config
  DB.runSqlite (dbFile c) $ do
   DB.runMigration migrateAll 
  s <- initAppState c
  let r m = runReaderT (runAppStateM m) s
  scottyT (port c) r app 

app :: ScottyT T.Text AppStateM ()
app = do
    get "/:set" getSetHandler
    get "/:set/:elem" getElemHandler
    put "/:set/:elem" putElementHandler
    delete "/:set/:elem" deleteElementHandler
    post "/:set" postSetHandler

getCurrentTimestamp :: IO L.TimeStamp
getCurrentTimestamp = do
    t <- getSystemTime
    return $ (toInteger (systemSeconds t) * 1000) + (toInteger ((systemNanoseconds t) `quot` 1000000))

modifyElement :: (StringSet -> String -> L.TimeStamp -> StringSet) -> SubHandler ()
modifyElement f = do
    setName <- param "set"
    elem <- param "elem"
    st <- lift ask
    upd <- liftIO $ do
        ts <- getCurrentTimestamp
        STM.atomically $ do 
            let setsVar = appSet st
            sets <- readTVar setsVar
            let current = M.findWithDefault (L.empty) setName sets
            let updated = f current elem ts
            writeTVar setsVar $ M.insert setName updated sets
            return updated
    runQuery (DB.repsert (SetModelKey setName) (SetModel upd))
    json True

putElementHandler :: Handler
putElementHandler = modifyElement L.insert

deleteElementHandler :: Handler
deleteElementHandler = modifyElement L.remove

postSetHandler :: Handler
postSetHandler = do 
    setName <- param "set"
    postedSet <- jsonData
    s <- lift $ asks appSet  
    (upd, changed) <- liftIO $ do
        STM.atomically $ do 
            sets <- readTVar s
            let current = M.findWithDefault (L.empty) setName sets
            let updated = L.merge current postedSet
            case L.LWWSetExact current == L.LWWSetExact updated of
                False -> do writeTVar s $ M.insert setName updated sets
                            return (updated, True)
                True -> return (current, False)
    when changed $ runQuery (DB.repsert (SetModelKey setName) (SetModel upd))
    json True

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
  , toReplicate :: STM.TVar (M.Map String StringSet)
  , dbPool :: DB.ConnectionPool
  }

newtype AppStateM a = AppStateM 
  { runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad,
    MonadIO, MonadReader AppState)

data Config = Config 
  { initialPeers :: [String] 
  , dbFile :: TS.Text
  , port :: Int
  , logging :: Bool
  , replicationDelay :: Int
  } deriving (Show, Data, Typeable)

config :: Config 
config = Config 
  { initialPeers = []
  , dbFile = "sqlite3.db" 
  , port = 3000
  , logging = False 
  , replicationDelay = 5  
  }

createDBPool :: Config -> IO DB.ConnectionPool
createDBPool c = do
    let connectionString = dbFile c
    let poolSize = 1
    if logging c then 
        runStdoutLoggingT (DB.createSqlitePool connectionString poolSize)
    else 
        runNoLoggingT (DB.createSqlitePool connectionString poolSize)

initAppState :: Config -> IO AppState
initAppState c = do
    p <- createDBPool c
    fromDb <- liftIO $ DB.runSqlPool (DB.selectList [] []) p
    let kv = map (\e -> (keyAsString $ DB.entityKey e, setModelSet $ DB.entityVal e)) fromDb
    sv <- STM.newTVarIO $ (M.fromList kv :: M.Map String StringSet) 
    rv <- STM.newTVarIO $ (M.empty :: M.Map String StringSet)
    let appState = AppState 
                    { appSet = sv
                    , toReplicate = rv
                    , dbPool = p
                    }
    ts <- getCurrentTimestamp
    atomically $ do
        sets <- readTVar sv
        let peersSet = (foldr (\e a -> L.insert a e ts) L.empty (initialPeers c) :: StringSet)
        writeTVar sv $ M.insertWith L.merge peerSetName peersSet sets
    allPeers <- getPeers appState
    liftIO $ DB.runSqlPool (DB.repsert (SetModelKey peerSetName) (SetModel allPeers)) p
    forkIO $ forever $ do 
        when (logging c) $ putStrLn "Running replication"
        runReplication appState
        threadDelay $ (replicationDelay c) * 1000000
    return appState

getPeers :: AppState -> IO StringSet
getPeers s = do
    let sv = appSet s
    sets <- readTVarIO sv
    return $ M.findWithDefault L.empty peerSetName sets

runReplication :: AppState -> IO ()
runReplication s = do
    outstanding <- atomically $ swapTVar (toReplicate s) M.empty
    peers <- getPeers s
    let peerSet = L.toSet peers
    forM_ peerSet $ (\peer -> do
        forM_ (M.toList outstanding) $ (\(setName, set) -> do
            W.post ("http://" ++ peer ++ "/" ++ setName) (toJSON set)))

keyAsString :: DB.Key SetModel -> String
keyAsString k = case head $ DB.keyToValues k of
    DB.PersistText t -> TS.unpack t
    _ -> error "Can't parse key"

runQuery :: DB.SqlPersistT IO a -> SubHandler a
runQuery q = do
    p <- lift (asks dbPool)
    liftIO $ DB.runSqlPool q p