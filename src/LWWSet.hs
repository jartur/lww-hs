{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module LWWSet(LWWSet, toSet, merge, empty, 
              unit, query, insert, remove,
              TimeStamp, LWWSetExact(..)
             ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as LBS
import Database.Persist.Class (PersistField(..))
import Database.Persist.Types (PersistValue(..), SqlType(..))
import Database.Persist.Sql (PersistFieldSql(..))

import GHC.Generics

type TimeStamp = Integer

-- Helper datastructure a set with every element having the latest update timestamp attached
data TimeStampedSet a = TimeStampedSet (M.Map a TimeStamp)
    deriving (Show, Generic)

-- The monoid instance allows us to create and merge sets
instance Ord a => Monoid (TimeStampedSet a) where 
    mempty = TimeStampedSet mempty
    mappend (TimeStampedSet left) (TimeStampedSet right) = TimeStampedSet $ M.unionWith max left right

instance ToJSONKey a => ToJSON (TimeStampedSet a) where
    toEncoding = genericToEncoding defaultOptions
instance (FromJSONKey a, Ord a) => FromJSON (TimeStampedSet a)

instance Ord a => Eq (TimeStampedSet a) where 
    (TimeStampedSet m1) == (TimeStampedSet m2) = m1 == m2

-- Check if element is in the set
tsSetQuery :: (Ord a) => TimeStampedSet a -> a -> Bool
tsSetQuery (TimeStampedSet m) x = M.member x m

-- Retrieve the element with its timestamp
tsSetLookup :: (Ord a) => TimeStampedSet a -> a -> Maybe (a, TimeStamp)
tsSetLookup (TimeStampedSet m) x = M.lookup x m >>= \ts -> return (x, ts)

-- Insert the element with a specified timestamp, updating the timestamp if element was already present
tsSetInsert :: (Ord a) => TimeStampedSet a -> a -> TimeStamp -> TimeStampedSet a
tsSetInsert (TimeStampedSet m) x ts = TimeStampedSet $ M.insert x ts m

-- | The LWW-Element-Set datastructure
data LWWSet a = LWWSet { lwwAdded :: TimeStampedSet a 
                        ,lwwRemoved :: TimeStampedSet a }
    deriving (Show, Generic)

-- | 'Monoid' instance is the essence of it being mergeable
instance Ord a => Monoid (LWWSet a) where 
    mempty = LWWSet mempty mempty
    mappend (LWWSet leftAdded leftRem) (LWWSet rightAdded rightRem) = 
        -- Basically we just merge add set and remove set using the previously defined monoid instance
        LWWSet (leftAdded `mappend` rightAdded) (leftRem `mappend` rightRem)

-- | We consider two 'LWWSet's equal if their reductions to normal 'S.Set' are equal
instance Ord a => Eq (LWWSet a) where
    s1 == s2 = (toSet s1) == (toSet s2)

-- | Newtype for exact equality
newtype LWWSetExact a = LWWSetExact (LWWSet a)

-- | This instance checks for exact equality of two sets.
--   They will be considered equal iff they have the same 
--   added elements, the same removed elements and all of 
--   the respective timestamps are equal.
instance Ord a => Eq (LWWSetExact a) where 
    (LWWSetExact (LWWSet added1 removed1)) == (LWWSetExact (LWWSet added2 removed2)) = 
        (added1 == added2) && (removed1 == removed2)

instance ToJSONKey a => ToJSON (LWWSet a) where
    toEncoding = genericToEncoding defaultOptions
instance (FromJSONKey a, Ord a) => FromJSON (LWWSet a)

instance (ToJSONKey a, FromJSONKey a, Ord a) => PersistField (LWWSet a) where 
    toPersistValue s = PersistText $ E.decodeUtf8 $ LBS.toStrict $ encode s
    fromPersistValue (PersistText j) = case decodeStrict $ E.encodeUtf8 j of
        Nothing -> Left "Cannot parse"
        Just s -> Right s
    fromPersistValue _ = Left "Cannot deserialize"

instance (ToJSONKey a, FromJSONKey a, Ord a) => PersistFieldSql (LWWSet a) where 
    sqlType _ = SqlString

-- | Reduce 'LWWSet' to a normal 'S.Set' which contains only non-deleted elements
toSet :: (Ord a) => LWWSet a -> S.Set a
toSet s@(LWWSet (TimeStampedSet addMap) _) = 
    M.foldMapWithKey present addMap
    where 
        present x _ = if query s x then S.singleton x else S.empty

-- | Here we use the fact that 'LWWSet' is a 'Monoid'
merge :: (Ord a) => LWWSet a -> LWWSet a -> LWWSet a
merge = mappend

-- | Construct an empty 'LWWSet', also from 'Monoid' instance
empty :: (Ord a) => LWWSet a
empty = mempty

-- | Construct an 'LWWSet' with a single element 
unit :: (Ord a) => a -> TimeStamp -> LWWSet a
unit x ts = insert mempty x ts

-- | Check if the element is in the 'LWWSet', which means it was added the latest add timestamp
-- is greater that the last removal if any. Here we are biased to removal.
query :: (Ord a) => LWWSet a -> a -> Bool
query (LWWSet addSet remSet) x = maybe False checkRemoval maybeAdded
    where 
        maybeAdded = tsSetLookup addSet x
        checkRemoval (_, addedTs) = 
            case tsSetLookup remSet x of
                Just (_, removedTs) -> removedTs < addedTs -- Biased to removal
                Nothing -> True

-- | Add new element to the 'LWWSet'                
insert :: (Ord a) => LWWSet a -> a -> TimeStamp -> LWWSet a
insert (LWWSet add rem) x ts = LWWSet (tsSetInsert add x ts) rem

-- | Remove an element from the 'LWWSet'
remove :: (Ord a) => LWWSet a -> a -> TimeStamp -> LWWSet a
remove (LWWSet add rem) x ts = LWWSet add (tsSetInsert rem x ts)
