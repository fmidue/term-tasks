module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List (transpose)
import Data.Maybe (fromJust)
import Control.Monad (replicateM,zipWithM)

oneValidTerm :: Signature -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) a b = arbTerm sig a b fs

arbTerm :: Signature -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm sig a b fs = do
  one <- elements fs
  let n = division (length (arguments one)) a b
  if null n
  then return Nothing
  else do
  n' <- elements n
  termList <- zipWithM (\(a',b') -> arbTerm sig a' b' . allSameTypes sig) n' (arguments one)
  let termList' = sequence termList
  case termList' of
    Nothing -> return Nothing
    Just _ -> return (Just (Term (symbol one) (fromJust termList')))

division ::Int -> Int -> Int -> [[(Int,Int)]]
division 0 1 _ = [[]]
division n a b
  | a > b  = []
  | n >= a = division n (n + 1) b
  | otherwise = filter isValidTuples ts
                    where ts = map (theTuples . newLists n) (division' n a b)

division' :: Int -> Int -> Int -> [[Int]]
division' n a b = [x ++ y | x <- validLists a (theLists n a), y <- validLists b (theLists n b)]

theLists ::Int -> Int -> [[Int]]
theLists n a = replicateM n [1..a-1]

validLists :: Int -> [[Int]] -> [[Int]]
validLists a = filter ((==a-1) . sum)

newLists :: Int -> [Int] -> [[Int]]
newLists n xs = a : [b]
                  where (a,b) = splitAt n xs

tuplify2 :: [Int] -> (Int,Int)
tuplify2 [x,y] = (x,y)
tuplify2 _ = error "This will never happen!"

theTuples :: [[Int]] -> [(Int,Int)]
theTuples xs = map tuplify2 (transpose xs)

isValidTuples :: [(Int,Int)] -> Bool
isValidTuples = all (uncurry (<=))



