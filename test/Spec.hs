{-# LANGUAGE TemplateHaskell #-}
module Main where
import LWWSet
import qualified Data.Set as S
import Test.QuickCheck.All 

prop_presentAfterInsertion :: Int -> Bool
prop_presentAfterInsertion x = lwwSetQuery (lwwSetUnit x 0) x

prop_idempotentInsertion :: Int -> Bool
prop_idempotentInsertion x = withX == withXX
    where withX = lwwSetUnit x 0
          withXX = lwwSetInsert withX x 1

prop_removalWithHigherTsWins :: Int -> Bool
prop_removalWithHigherTsWins x = not $ lwwSetQuery (lwwSetRemove (lwwSetUnit x 0) x 1) x

prop_removalWithLowerTsLoses :: Int -> Bool
prop_removalWithLowerTsLoses x = lwwSetQuery (lwwSetRemove (lwwSetUnit x 1) x 0) x

prop_biasedForRemoval :: Int -> Bool
prop_biasedForRemoval x = not $ lwwSetQuery (lwwSetRemove (lwwSetUnit x 1) x 1) x

prop_commutativeRemovalWithHigherTs :: Int -> Bool
prop_commutativeRemovalWithHigherTs x = removeInsert == insertRemove
    where
        removeInsert = lwwSetInsert (lwwSetRemove lwwSetEmpty x 1) x 0
        insertRemove = lwwSetRemove (lwwSetInsert lwwSetEmpty x 0) x 1

prop_multipleAdditions :: Int -> Int -> Bool
prop_multipleAdditions x y = (lwwSetToSet (lwwSetInsert (lwwSetUnit x 0) y 1)) == (S.fromList [x, y])

prop_emptyIsEmpty :: Int -> Bool
prop_emptyIsEmpty x = not $ lwwSetQuery lwwSetEmpty x

prop_insertionAfterRemoval :: Int -> Bool
prop_insertionAfterRemoval x = lwwSetQuery (lwwSetInsert (lwwSetRemove (lwwSetUnit x 0) x 1) x 2) x

prop_mergeCommutes :: Int -> Int -> Bool
prop_mergeCommutes x y = lwwSetMerge left right == lwwSetMerge right left 
    where
        left = lwwSetUnit x 0
        right = lwwSetUnit y 1

prop_mergeLinearizesHistory :: Int -> Int -> Bool
prop_mergeLinearizesHistory x y = lwwSetMerge left right == linear
    where
        left = lwwSetRemove (lwwSetInsert lwwSetEmpty x 0) y 1
        right = lwwSetInsert (lwwSetRemove lwwSetEmpty x 2) y 3
        linear = lwwSetInsert (lwwSetRemove (lwwSetRemove (lwwSetInsert lwwSetEmpty x 0) y 1) x 2) y 3

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  runTests
  return ()