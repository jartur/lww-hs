{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module SqlModel where
import Database.Persist.TH
import Data.Text
import LWWSet (LWWSet)

-- | Just a simple wrapper to use with Sqlite backend
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SetModel
    Id String
    set (LWWSet String)
    deriving Show
|]