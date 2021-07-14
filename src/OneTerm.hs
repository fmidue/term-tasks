module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List (transpose)

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig@(Signature fs) = arbTerm sig fs

arbTerm :: Signature -> [FunctionSymbol] -> Gen Term
arbTerm sig fs = do
  one <- elements fs
  termList <- mapM (arbTerm sig . allSameTypes sig) (arguments one)
  return (Term (symbol one) termList)

division ::Int -> Int -> Int -> [[(Int,Int)]]
division n a b = filter isValidTuples ts
                    where ts = map (theTuples . newLists n) (division' n a b)

division' :: Int -> Int -> Int -> [[Int]]
division' n a b = [x ++ y | x <- validLists a (sequence (theLists n a)), y <- validLists b (sequence (theLists n b))]

theLists ::Int -> Int -> [[Int]]
theLists 0 _ = []
theLists n a = [1..a-1] : theLists (n-1) a

validLists :: Int -> [[Int]] -> [[Int]]
validLists a = filter ((==a-1) . sum)

newLists :: Int -> [Int] -> [[Int]]
newLists n xs = take n xs : [drop n xs]

tuplify2 :: [Int] -> (Int,Int)
tuplify2 [] = error "This will never happen!"
tuplify2 [x,y] = (x,y)

theTuples :: [[Int]] -> [(Int,Int)]
theTuples xs = map tuplify2 (transpose xs)

isValidTuples :: [(Int,Int)] -> Bool
isValidTuples = all (uncurry (<=))
