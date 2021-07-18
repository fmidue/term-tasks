module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List (transpose)
import Data.Maybe (catMaybes,isNothing)
import Control.Monad (replicateM)

oneValidTerm :: Signature -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) a b = arbTerm sig a b fs

arbTerm :: Signature -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm sig a b fs = do
  one <- elements fs
  let n = division (length (arguments one)) a b
  if a <= 1 && null (arguments one)
  then return (Just (Term (symbol one) []))
  else if null n
  then return Nothing
  else do
  n' <- elements n
  termList <- mapM (\(t,(a',b')) -> arbTerm sig a' b' . allSameTypes sig $ t) (zip (arguments one) n')
  if isAllValiable termList
  then return (Just (Term (symbol one) (catMaybes termList)))
  else return Nothing

division ::Int -> Int -> Int -> [[(Int,Int)]]
division n a b = filter isValidTuples ts
                    where ts = map (theTuples . newLists n) (division' n a b)

division' :: Int -> Int -> Int -> [[Int]]
division' n a b = [x ++ y | x <- validLists a (theLists n a), y <- validLists b (theLists n b)]

theLists ::Int -> Int -> [[Int]]
theLists n a = replicateM n [0..a-1]

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

number :: Term -> Int
number fs = 1 + number' (parameters fs)

number' :: [Term] -> Int
number' [] = 0
number' (f:fs)
  | null (parameters f) = 1 + number' fs
  | otherwise = 1 + number' (parameters f) + number' fs

isAllValiable :: [Maybe Term] -> Bool
isAllValiable [] = True
isAllValiable (t:ts)
  | isNothing t = False
  | otherwise = isAllValiable ts

