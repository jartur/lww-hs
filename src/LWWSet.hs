module LWWSet where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import Data.Maybe

type TimeStamp = Integer

-- Helper datastructure a set with every element having the latest update timestamp attached
data TimeStampedSet a = TimeStampedSet (M.Map a TimeStamp)
    deriving (Show)

-- The monoid instance allows us to create and merge sets
instance Ord a => Monoid (TimeStampedSet a) where 
    mempty = TimeStampedSet mempty
    mappend (TimeStampedSet left) (TimeStampedSet right) = TimeStampedSet $ M.unionWith max left right

-- Check if element is in the set
tsSetQuery :: (Ord a) => TimeStampedSet a -> a -> Bool
tsSetQuery (TimeStampedSet m) x = M.member x m

-- Retrieve the element with its timestamp
tsSetLookup :: (Ord a) => TimeStampedSet a -> a -> Maybe (a, TimeStamp)
tsSetLookup (TimeStampedSet m) x = M.lookup x m >>= \ts -> return (x, ts)

-- Insert the element with a specified timestamp, updating the timestamp if element was already present
tsSetInsert :: (Ord a) => TimeStampedSet a -> a -> TimeStamp -> TimeStampedSet a
tsSetInsert (TimeStampedSet m) x ts = TimeStampedSet $ M.insert x ts m

-- The LWW-Element-Set datastructure
data LWWSet a = LWWSet { lwwAdded :: TimeStampedSet a 
                        ,lwwRemoved :: TimeStampedSet a }
    deriving (Show)

-- Monoid instance is the essence of it being mergeable
instance Ord a => Monoid (LWWSet a) where 
    mempty = LWWSet mempty mempty
    mappend (LWWSet leftAdded leftRem) (LWWSet rightAdded rightRem) = 
        -- Basically we just merge add set and remove set using the previously defined monoid instance
        LWWSet (leftAdded `mappend` rightAdded) (leftRem `mappend` rightRem)

-- We consider two LWW sets equal if their reductions to normal sets are equal
instance (Eq a, Ord a) => Eq (LWWSet a) where
    s1 == s2 = (lwwSetToSet s1) == (lwwSetToSet s2)

-- Reduce LWW set to a normal set whic contains only non-deleted elements
lwwSetToSet :: (Ord a) => LWWSet a -> S.Set a
lwwSetToSet s@(LWWSet (TimeStampedSet addMap) _) = 
    M.foldMapWithKey present addMap
    where 
        present x _ = if lwwSetQuery s x then S.singleton x else S.empty

-- Here we use the fact that LWWSet is a monoid
lwwSetMerge :: (Ord a) => LWWSet a -> LWWSet a -> LWWSet a
lwwSetMerge = mappend

-- Construct an empty set, also from monoid
lwwSetEmpty :: (Ord a) => LWWSet a
lwwSetEmpty = mempty

-- Construct an LWW set with a single element 
lwwSetUnit :: (Ord a) => a -> TimeStamp -> LWWSet a
lwwSetUnit x ts = lwwSetInsert mempty x ts

-- Check if the element is in the LWW set, which means it was added the latest add timestamp
-- is greater that the last removal if any. Here we are biased to removal.
lwwSetQuery :: (Ord a) => LWWSet a -> a -> Bool
lwwSetQuery (LWWSet addSet remSet) x = maybe False checkRemoval maybeAdded
    where 
        maybeAdded = tsSetLookup addSet x
        checkRemoval (_, addedTs) = 
            case tsSetLookup remSet x of
                Just (_, removedTs) -> removedTs < addedTs -- Biased to removal
                Nothing -> True

-- Add new element to the LWW set                
lwwSetInsert :: (Ord a) => LWWSet a -> a -> TimeStamp -> LWWSet a
lwwSetInsert (LWWSet add rem) x ts = LWWSet (tsSetInsert add x ts) rem

-- Remove an element from the LWW set
lwwSetRemove :: (Ord a) => LWWSet a -> a -> TimeStamp -> LWWSet a
lwwSetRemove (LWWSet add rem) x ts = LWWSet add (tsSetInsert rem x ts)