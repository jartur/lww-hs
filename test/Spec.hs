{-# LANGUAGE TemplateHaskell #-}
module Main where
import qualified LWWSet as L
import qualified Data.Set as S
import Test.QuickCheck.All 

prop_presentAfterInsertion :: Int -> Bool
prop_presentAfterInsertion x = L.query (L.unit x 0) x

prop_idempotentInsertion :: Int -> Bool
prop_idempotentInsertion x = withX == withXX
    where withX = L.unit x 0
          withXX = L.insert withX x 1

prop_removalWithHigherTsWins :: Int -> Bool
prop_removalWithHigherTsWins x = not $ L.query (L.remove (L.unit x 0) x 1) x

prop_removalWithLowerTsLoses :: Int -> Bool
prop_removalWithLowerTsLoses x = L.query (L.remove (L.unit x 1) x 0) x

prop_biasedForRemoval :: Int -> Bool
prop_biasedForRemoval x = not $ L.query (L.remove (L.unit x 1) x 1) x

prop_commutativeRemovalWithHigherTs :: Int -> Bool
prop_commutativeRemovalWithHigherTs x = removeInsert == insertRemove
    where
        removeInsert = L.insert (L.remove L.empty x 1) x 0
        insertRemove = L.remove (L.insert L.empty x 0) x 1

prop_multipleAdditions :: Int -> Int -> Bool
prop_multipleAdditions x y = (L.toSet (L.insert (L.unit x 0) y 1)) == (S.fromList [x, y])

prop_emptyIsEmpty :: Int -> Bool
prop_emptyIsEmpty x = not $ L.query L.empty x

prop_insertionAfterRemoval :: Int -> Bool
prop_insertionAfterRemoval x = L.query (L.insert (L.remove (L.unit x 0) x 1) x 2) x

prop_mergeCommutes :: Int -> Int -> Bool
prop_mergeCommutes x y = L.merge left right == L.merge right left 
    where
        left = L.unit x 0
        right = L.unit y 1

prop_mergeLinearizesHistory :: Int -> Int -> Bool
prop_mergeLinearizesHistory x y = L.merge left right == linear
    where
        left = L.remove (L.insert L.empty x 0) y 1
        right = L.insert (L.remove L.empty x 2) y 3
        linear = L.insert (L.remove (L.remove (L.insert L.empty x 0) y 1) x 2) y 3

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  runTests
  return ()
