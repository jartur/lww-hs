{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Exception (catch, SomeException)
import qualified Network.Wreq as W

import qualified Database.Persist as DB
import qualified Database.Persist.Sqlite as DB

import System.Console.CmdArgs

type StringSet = L.LWWSet String
type Handler = ActionT T.Text AppStateM ()
type SubHandler a = ActionT T.Text AppStateM a

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
            modifyInTVar (toReplicate st) setName (\current -> f current elem ts)
            modifyInTVar (appSet st) setName (\current -> f current elem ts)
    runQuery (DB.repsert (SetModelKey setName) (SetModel upd))
    json True

modifyInTVar :: TVar (M.Map String StringSet) -> String -> (StringSet -> StringSet) -> STM StringSet
modifyInTVar setsVar setName f = do
    sets <- readTVar setsVar
    let current = M.findWithDefault (L.empty) setName sets
    let updated = f current
    writeTVar setsVar $ M.insert setName updated sets
    return updated

putElementHandler :: Handler
putElementHandler = modifyElement L.insert

deleteElementHandler :: Handler
deleteElementHandler = modifyElement L.remove

postSetHandler :: Handler
postSetHandler = do 
    setName <- param "set"
    postedSet <- jsonData
    s <- lift $ ask  
    (upd, changed) <- liftIO $ do
        STM.atomically $ do 
            (updated, changed) <- modifySetInTVar (appSet s) setName (\current -> L.merge current postedSet)
            when changed $ modifySetInTVar (toReplicate s) setName (\current -> L.merge current postedSet) >> return ()
            return (updated, changed)
    when changed $ runQuery (DB.repsert (SetModelKey setName) (SetModel upd))
    json True

modifySetInTVar :: TVar (M.Map String StringSet) -> String -> (StringSet -> StringSet) -> STM (StringSet, Bool)
modifySetInTVar s setName f = do
    sets <- readTVar s
    let current = M.findWithDefault (L.empty) setName sets
    let updated = f current
    case L.LWWSetExact current == L.LWWSetExact updated of
        False -> do writeTVar s $ M.insert setName updated sets
                    return (updated, True)
        True -> return (current, False)

getSet :: SubHandler (Maybe StringSet)
getSet = do
    setName <- param "set"
    s <- lift (asks appSet)
    liftIO $ do 
        sets <- readTVarIO s
        return $ M.lookup setName sets

getSetHandler :: Handler
getSetHandler = do
    ls <- getSet
    case ls of
        Nothing -> do
            status notFound404
            json Null
        Just res ->  
            json $ res

getElemHandler :: Handler
getElemHandler = do
     elem <- param "elem"
     ls <- getSet
     case ls of
        Nothing -> do
            status notFound404
            json Null
        Just res ->  
            json $ L.query res elem

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
  , fullReplicationDelay :: Int
  } deriving (Show, Data, Typeable)

config :: Config 
config = Config 
  { initialPeers = []
  , dbFile = "sqlite3.db" 
  , port = 3000
  , logging = False 
  , replicationDelay = 5
  , fullReplicationDelay = 60 
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
        let merged = M.insertWith L.merge peerSetName peersSet sets
        writeTVar sv $ merged
        writeTVar rv $ merged
    allPeers <- getPeers appState
    liftIO $ DB.runSqlPool (DB.repsert (SetModelKey peerSetName) (SetModel allPeers)) p
    forkIO $ forever $ do 
        when (logging c) $ putStrLn "Running replication"
        runReplication appState
        threadDelay $ (replicationDelay c) * 1000000
    forkIO $ forever $ do 
        when (logging c) $ putStrLn "Running full replication"
        runFullReplication appState
        threadDelay $ (fullReplicationDelay c) * 1000000 
    return appState

getPeers :: AppState -> IO StringSet
getPeers s = do
    sets <- readTVarIO (appSet s)
    return $ M.findWithDefault L.empty peerSetName sets

remPeer :: AppState -> String -> IO ()
remPeer s peer = do
    putStrLn $ "removing peer " ++ peer
    ts <- getCurrentTimestamp
    atomically $ modifyInTVar (appSet s) peerSetName (\peers -> L.remove peers peer ts)
    return ()

runReplication :: AppState -> IO ()
runReplication s = do
    outstanding <- atomically $ swapTVar (toReplicate s) M.empty
    peers <- getPeers s
    replicateSets s outstanding peers

runFullReplication :: AppState -> IO ()
runFullReplication s = do
    sets <- readTVarIO (appSet s)
    peers <- getPeers s
    replicateSets s sets peers

replicateSets :: AppState -> (M.Map String StringSet) -> StringSet -> IO ()
replicateSets s sets peers = do
    let peerSet = L.toSet peers
    forM_ peerSet $ (\peer -> do
        forM_ (M.toList sets) $ (\(setName, set) -> do
            catch ((W.post ("http://" ++ peer ++ "/" ++ setName) (toJSON set)) >>= putStrLn . show)
                  (\(e :: SomeException) -> remPeer s peer)))

keyAsString :: DB.Key SetModel -> String
keyAsString k = case head $ DB.keyToValues k of
    DB.PersistText t -> TS.unpack t
    _ -> error "Can't parse key"

runQuery :: DB.SqlPersistT IO a -> SubHandler a
runQuery q = do
    p <- lift (asks dbPool)
    liftIO $ DB.runSqlPool q p