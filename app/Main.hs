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

-- | Just a constant for a special set that we use for peer discovery
peerSetName :: String
peerSetName = "_peers_"

-- | Program entry point
main :: IO ()
main = do 
  conf <- cmdArgs config -- parse command line
  DB.runSqlite (dbFile conf) $ do -- run database migration
   DB.runMigration migrateAll 
  state <- initAppState conf 
  -- This is where we construct a Monad Transformer stack for the Scotty app
  let r m = runReaderT (runAppStateM m) state
  scottyT (port conf) r app 

-- | Main Scotty configuration
app :: ScottyT T.Text AppStateM ()
app = do
    get "/:set" getSetHandler
    get "/:set/:elem" getElemHandler
    put "/:set/:elem" putElementHandler
    delete "/:set/:elem" deleteElementHandler
    post "/:set" postSetHandler

-- | Current timestamp in ms precision
getCurrentTimestamp :: IO L.TimeStamp
getCurrentTimestamp = do
    t <- getSystemTime
    return $ (toInteger (systemSeconds t) * 1000) + (toInteger ((systemNanoseconds t) `quot` 1000000))

-- | Helper Scotty Action to modify a set on an element basis.
modifyElement :: (StringSet -> String -> L.TimeStamp -> StringSet) -> SubHandler ()
modifyElement f = do
    setName <- param "set"
    elem <- param "elem"
    state <- lift ask
    upd <- liftIO $ do
        ts <- getCurrentTimestamp
        STM.atomically $ do 
            modifyInTVar (toReplicate state) setName (\current -> f current elem ts)
            modifyInTVar (appSet state) setName (\current -> f current elem ts)
    runQuery (DB.repsert (SetModelKey setName) (SetModel upd))
    json True

-- | Modifies a specified set in map in a 'TVar'. Returns the updated set.
modifyInTVar :: TVar (M.Map String StringSet) -> String -> (StringSet -> StringSet) -> STM StringSet
modifyInTVar setsVar setName f = do
    sets <- readTVar setsVar
    let current = M.findWithDefault (L.empty) setName sets
    let updated = f current
    writeTVar setsVar $ M.insert setName updated sets
    return updated

-- | Action that will insert the element
putElementHandler :: Handler
putElementHandler = modifyElement L.insert

-- | Action that will delete the element
deleteElementHandler :: Handler
deleteElementHandler = modifyElement L.remove

-- | Merge a full set from a post request
postSetHandler :: Handler
postSetHandler = do 
    setName <- param "set"
    postedSet <- jsonData
    state <- lift $ ask  
    (upd, changed) <- liftIO $ do
        STM.atomically $ do 
            (updated, changed) <- modifySetInTVarIfChanged (appSet state) setName (\current -> L.merge current postedSet)
            when changed $ modifySetInTVarIfChanged (toReplicate state) setName (\current -> L.merge current postedSet) >> return ()
            return (updated, changed)
    when changed $ runQuery (DB.repsert (SetModelKey setName) (SetModel upd))
    json True

-- | Will modify the specified set but will only update the TVar if content changed.
--   Returns new set and the flag signaling if any change has happened.
--   This is useful for merging sets that have been already merged so that
--   we can avoid taking any actions for these cases (e.g. writing to the DB or replicating).
modifySetInTVarIfChanged :: TVar (M.Map String StringSet) -> String -> (StringSet -> StringSet) -> STM (StringSet, Bool)
modifySetInTVarIfChanged state setName f = do
    sets <- readTVar state
    let current = M.findWithDefault (L.empty) setName sets
    let updated = f current
    case L.LWWSetExact current == L.LWWSetExact updated of
        False -> do writeTVar state $ M.insert setName updated sets
                    return (updated, True)
        True -> return (current, False)

-- | Helper handler to retrieve a set
getSet :: SubHandler (Maybe StringSet)
getSet = do
    setName <- param "set"
    state <- lift (asks appSet)
    liftIO $ do 
        sets <- readTVarIO state
        return $ M.lookup setName sets

-- | Get the whole set        
getSetHandler :: Handler
getSetHandler = do
    ls <- getSet
    case ls of
        Nothing -> do
            status notFound404
            json Null
        Just res ->  
            json $ res

-- | Query for an element in a set
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

-- | Utility function to init a pool of connections to SQLite            
createDBPool :: Config -> IO DB.ConnectionPool
createDBPool conf = do
    let connectionString = dbFile conf
    let poolSize = 1
    if logging conf then 
        runStdoutLoggingT (DB.createSqlitePool connectionString poolSize)
    else 
        runNoLoggingT (DB.createSqlitePool connectionString poolSize)

-- | Prepare the whole app context        
initAppState :: Config -> IO AppState
initAppState conf = do
    p <- createDBPool conf
    -- Read all the database in memory
    -- It may be not that wise for a production quality project
    -- OTOH people do use Redis in production
    fromDb <- liftIO $ DB.runSqlPool (DB.selectList [] []) p
    let kv = map (\e -> (keyAsString $ DB.entityKey e, setModelSet $ DB.entityVal e)) fromDb
    -- Just setup some references
    sv <- STM.newTVarIO $ (M.fromList kv :: M.Map String StringSet) 
    rv <- STM.newTVarIO $ (M.empty :: M.Map String StringSet)
    let appState = AppState 
                    { appSet = sv
                    , toReplicate = rv
                    , dbPool = p
                    }
    addInitialPeers sv rv
    -- Dump current peer list to the database
    allPeers <- getPeers appState
    liftIO $ DB.runSqlPool (DB.repsert (SetModelKey peerSetName) (SetModel allPeers)) p
    -- Start replication processes
    forkReplicator runReplication (replicationDelay conf) appState
    forkReplicator runFullReplication (fullReplicationDelay conf) appState
    return appState
    where
        addInitialPeers sv rv = do
            ts <- getCurrentTimestamp
            atomically $ do
                sets <- readTVar sv
                let peersSet = (foldr (\e a -> L.insert a e ts) L.empty (initialPeers conf) :: StringSet)
                let merged = M.insertWith L.merge peerSetName peersSet sets
                writeTVar sv $ merged
                writeTVar rv $ merged
        forkReplicator repFun delay appState = forkIO $ forever $ do
            when (logging conf) $ putStrLn "Running replication"
            repFun appState
            threadDelay $ delay * 1000000

-- | Current list of peers
getPeers :: AppState -> IO StringSet
getPeers state = do
    sets <- readTVarIO (appSet state)
    return $ M.findWithDefault L.empty peerSetName sets

-- | Remove a peer from the current list    
remPeer :: AppState -> String -> IO ()
remPeer state peer = do
    putStrLn $ "removing peer " ++ peer
    ts <- getCurrentTimestamp
    atomically $ modifyInTVar (appSet state) peerSetName (\peers -> L.remove peers peer ts)
    return ()

-- | Perform partial replication to all peers. 
--   This will replicate all the local changes that happened since the last replication.  
runReplication :: AppState -> IO ()
runReplication state = do
    outstanding <- atomically $ swapTVar (toReplicate state) M.empty
    peers <- getPeers state
    replicateSets state outstanding peers

-- | Run full replication to all peers.    
runFullReplication :: AppState -> IO ()
runFullReplication state = do
    sets <- readTVarIO (appSet state)
    peers <- getPeers state
    replicateSets state sets peers

-- | Send sets to merge to other peers. Will send to itself, which definitely can be improved.    
replicateSets :: AppState -> (M.Map String StringSet) -> StringSet -> IO ()
replicateSets state sets peers = do
    let peerSet = L.toSet peers
    forM_ peerSet $ (\peer -> do
        forM_ (M.toList sets) $ (\(setName, set) -> do
            catch ((W.post ("http://" ++ peer ++ "/" ++ setName) (toJSON set)) >>= putStrLn . show)
                  (\(e :: SomeException) -> remPeer state peer)))

-- | Utility function to extract key as a set name                  
keyAsString :: DB.Key SetModel -> String
keyAsString k = case head $ DB.keyToValues k of
    DB.PersistText t -> TS.unpack t
    _ -> error "Can't parse key"

-- | Run database query    
runQuery :: DB.SqlPersistT IO a -> SubHandler a
runQuery q = do
    p <- lift (asks dbPool)
    liftIO $ DB.runSqlPool q p

-- Useful type aliases    
type StringSet = L.LWWSet String
type Handler = ActionT T.Text AppStateM ()
type SubHandler a = ActionT T.Text AppStateM a

-- | Holds all the application state
data AppState = AppState 
  { appSet :: STM.TVar (M.Map String StringSet)
  , toReplicate :: STM.TVar (M.Map String StringSet)
  , dbPool :: DB.ConnectionPool
  }

-- | Newtype for our state Reader stack
newtype AppStateM a = AppStateM 
  { runAppStateM :: ReaderT AppState IO a
  } deriving (Applicative, Functor, Monad,
    MonadIO, MonadReader AppState)

-- | Command line parameters data 
data Config = Config 
  { initialPeers :: [String] 
  , dbFile :: TS.Text
  , port :: Int
  , logging :: Bool
  , replicationDelay :: Int
  , fullReplicationDelay :: Int
  } deriving (Show, Data, Typeable)

-- | Default values for the command line parameters
config :: Config 
config = Config 
    { initialPeers = []
    , dbFile = "sqlite3.db" 
    , port = 3000
    , logging = False 
    , replicationDelay = 5
    , fullReplicationDelay = 60 
    }