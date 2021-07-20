{-# LANGUAGE DuplicateRecordFields#-}
module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List (transpose)
import Data.Maybe (fromJust)
import Control.Monad (replicateM,zipWithM)

oneValidTerm :: Signature -> Mode -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) m a b = arbTerm sig (Mode' m (theSymbol m)) a b fs

arbTerm :: Signature -> Mode' -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm sig m@(Mode' _ (Just s)) a b fs = do
  x <- selectOne m fs
  let (one,newMode) = fromJust x
      n = division (length ((arguments :: FunctionSymbol -> [Type]) one)) a b
  if null ((arguments :: FunctionSymbol -> [Type]) one) && newMode == Mode' (ONCE s) (Just s)
  then return Nothing
  else do
  if null n
  then return Nothing
  else do
  n' <- elements n
  if newMode == Mode' (ONCE s) (Just s)
  then do
  y <- chooseInt (0,length ((arguments :: FunctionSymbol -> [Type]) one))
  termList <- tt sig n' ((arguments :: FunctionSymbol -> [Type]) one) (Just s) y
  let termList' = sequence termList
  case termList' of
    Nothing -> return Nothing
    Just ts -> return (Just (Term ((symbol :: FunctionSymbol -> String) one) ts))
  else do
  termList <- zipWithM (\(a',b') -> arbTerm sig newMode a' b' . allSameTypes sig) n' ((arguments :: FunctionSymbol -> [Type]) one)
  let termList' = sequence termList
  case termList' of
    Nothing -> return Nothing
    Just ts -> return (Just (Term ((symbol :: FunctionSymbol -> String) one) ts))

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

selectOne ::Mode' -> [FunctionSymbol] -> Gen (Maybe (FunctionSymbol,Mode'))
selectOne _ [] = return Nothing
selectOne (Mode' (NO _) Nothing) _ = return Nothing
selectOne (Mode' (ONCE _) Nothing) _ = return Nothing
selectOne m@(Mode' NONE _) fs = do one <- elements fs
                                   return (Just(one,m))
selectOne m@(Mode' (NO _) (Just s)) fs = do if null fs'
                                            then return Nothing
                                            else do one <- elements fs'
                                                    return (Just(one,m))
                                              where fs' = filter (\x -> (symbol :: FunctionSymbol -> String) x /= s) fs
selectOne m@(Mode' (ONCE _) (Just s)) fs = do one <- elements fs
                                              if (symbol :: FunctionSymbol -> String) one == s
                                              then return (Just(one,Mode' (NO s) (Just s)))
                                              else return (Just(one,m))

tt :: Signature -> [(Int,Int)] -> [Type] -> (Maybe String) -> Int -> Gen [Maybe Term]
tt _ [] _ _ _ = return []
tt _ _ [] _ _ = return []
tt _ _ _ Nothing _ = return []
tt sig ((a,b):ls) (t:ts) s'@(Just s) 0 = do x <- arbTerm sig (Mode' (ONCE s) s') a b (allSameTypes sig t)
                                            y <- tt sig ls ts s' (-1)
                                            return (x:y)
tt sig ((a,b):ls) (t:ts) s'@(Just s) n = do x <- arbTerm sig (Mode' (NO s) s') a b (allSameTypes sig t)
                                            y <- tt sig ls ts s' (n-1)
                                            return (x:y)




