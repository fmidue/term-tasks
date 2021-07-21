module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List (transpose)
import Control.Monad (replicateM,zipWithM)

oneValidTerm :: Signature -> Mode -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) m a b = arbTerm sig m a b fs

arbTerm :: Signature -> Mode -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm sig m a b fs = do
-- select one function symbol from fs. If symbol of "one" is the given symbol, newMode is changed from ONCE to NO
  x <- selectOne m fs
  case x of
    Nothing -> return Nothing
                             -- Here is the leaf node. When "one" is constant and it doesn't choose that symbol, return Nothing.
    Just (one,newMode) -> do if null ((arguments :: FunctionSymbol -> [Type]) one) && newMode == ONCE (symbol' m)
                             then return Nothing
                             else do
                             -- Here do as the original vision
                             let n = division (length ((arguments :: FunctionSymbol -> [Type]) one)) a b
                             if null n
                             then return Nothing
                             else do
                             n' <- elements n
                             -- Here need to seperate two different situations.
                             -- If ONCE, give ONCE to one sub node, others are all NO
                             if newMode == ONCE (symbol' m)
                             then do
                               -- random select one sub term. Here I think (0,0) is impossible, Because when one is constant
                               -- and ONCE, directly return Nothing
                               number <- chooseInt (0,length ((arguments :: FunctionSymbol -> [Type]) one))
                               termList <- newTermList sig n' ((arguments :: FunctionSymbol -> [Type]) one) (symbol' m) number
                               let termList' = sequence termList
                               case termList' of
                                 Nothing -> return Nothing
                                 Just ts -> return (Just (Term (symbol one) ts))
                               -- If NO and NONE, do as original version
                             else do
                               termList <- zipWithM (\(a',b') -> arbTerm sig newMode a' b' . allSameTypes sig) n' ((arguments :: FunctionSymbol -> [Type]) one)
                               let termList' = sequence termList
                               case termList' of
                                 Nothing -> return Nothing
                                 Just ts -> return (Just (Term (symbol one) ts))


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

selectOne ::Mode -> [FunctionSymbol] -> Gen (Maybe (FunctionSymbol,Mode))
selectOne _ [] = return Nothing
selectOne (NONE s) fs = do one <- elements fs
                           return (Just(one,NONE s))
selectOne (NO s'@(Just s)) fs = do if null fs'
                                   then return Nothing
                                   else do one <- elements fs'
                                           return (Just(one,NO s'))
                                     where fs' = filter (\x -> (symbol x) /= s) fs
-- if the given symbol is just seleted, the Mode change to NO and return
selectOne (ONCE s'@(Just s)) fs = do one <- elements fs
                                     if symbol one == s
                                     then return (Just(one,NO s'))
                                     else return (Just(one,ONCE s'))
selectOne _ _ = return Nothing

newTermList :: Signature -> [(Int,Int)] -> [Type] -> (Maybe String) -> Int -> Gen [Maybe Term]
newTermList _ [] _ _ _ = return []
newTermList _ _ [] _ _ = return []
-- define Mode of the selected node to ONCE
newTermList sig ((a,b):ls) (t:ts) s 0 = do x <- arbTerm sig (ONCE s) a b (allSameTypes sig t)
                                           y <- newTermList sig ls ts s (-1)
                                           return (x:y)
-- define Mode of rest nodes as NO
newTermList sig ((a,b):ls) (t:ts) s n = do x <- arbTerm sig (NO s) a b (allSameTypes sig t)
                                           y <- newTermList sig ls ts s (n-1)
                                           return (x:y)

