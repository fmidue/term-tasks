{-# LANGUAGE DuplicateRecordFields#-}
module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import DealWithTerm (termSymbols,termSymbols')
import Data.List (transpose)
import Data.Maybe (fromJust,isJust)
import Control.Monad (replicateM,zipWithM)

oneValidTerm :: Signature -> Mode -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) m@(ONCE s) a b = do term <- arbTerm sig m a b fs
                                                    if isJust term && (s `elem` termSymbols (fromJust term))
                                                    then return term
                                                    else return Nothing

oneValidTerm sig@(Signature fs) m a b = arbTerm sig m a b fs

arbTerm :: Signature -> Mode -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm sig m a b fs = do
  (one,newMode) <- selectOne m fs
  let n = division (length ((arguments :: FunctionSymbol -> [Type]) one)) a b
  if null n
  then return Nothing
  else do
  n' <- elements n
  termList <- zipWithM (\(a',b') -> arbTerm sig newMode a' b' . allSameTypes sig) n' ((arguments :: FunctionSymbol -> [Type]) one)
  let termList' = sequence termList
  case termList' of
    Nothing -> return Nothing
    Just ts -> do let x = termSymbols' ts
                  if count ((symbol :: Mode -> String) newMode) x >1
                  then return Nothing
                  else return (Just (Term ((symbol :: FunctionSymbol -> String) one) ts))

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

selectOne ::Mode -> [FunctionSymbol] -> Gen (FunctionSymbol,Mode)
selectOne NONE fs = do one <- elements fs
                       return (one,NONE)
selectOne (NO s) fs = do one <- elements fs'
                         return (one,NO s)
                           where fs' = filter (\x -> (symbol :: FunctionSymbol -> String) x /= s) fs
selectOne (ONCE s) fs = do one <- elements fs
                           if (symbol :: FunctionSymbol -> String) one == s
                           then return (one,NO s)
                           else return (one,ONCE s)

count :: String -> [String] -> Int
count x = length . filter (x==)

