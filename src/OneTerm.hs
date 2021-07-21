module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List (transpose)
import Data.Maybe (fromJust)
import Control.Monad (replicateM)

oneValidTerm :: Signature -> Mode -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) m a b = arbTerm sig m a b fs

arbTerm :: Signature -> Mode -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm sig m a b fs = do
-- select one function symbol from fs. If symbol of "one" is the given symbol, newMode is changed from ONCE to NO
  x <- selectOne m fs
  case x of
    Nothing -> return Nothing
    -- Here is the leaf node. When "one" is constant and it doesn't choose that symbol, return Nothing.
    Just (one,newMode) -> do
      if null ((arguments :: FunctionSymbol -> [Type]) one) && newMode == ONCE (fromJust(theSymbol m))
      then return Nothing
      -- Here do as the original vision
      else do
        let n = division (length ((arguments :: FunctionSymbol -> [Type]) one)) a b
        if null n
        then return Nothing
        else do
          n' <- elements n
          let l = length ((arguments :: FunctionSymbol -> [Type]) one)
          modeList <- newModes l newMode
          -- zip 3 :: [a] -> [b] -> [c] -> [(a,b,c)]
          termList <- mapM (\(ml,t,(a',b')) -> arbTerm sig ml a' b' . allSameTypes sig $t) (zip3 modeList ((arguments :: FunctionSymbol -> [Type]) one) n')
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
selectOne NONE fs = do one <- elements fs
                       return (Just(one,NONE))
selectOne (NO s) fs = do if null fs'
                         then return Nothing
                         else do one <- elements fs'
                                 return (Just(one,NO s))
                                   where fs' = filter (\x -> symbol x /= s) fs
-- if the given symbol is just seleted, the Mode change to NO and return
selectOne (ONCE s) fs = do one <- elements fs
                           if symbol one == s
                           then return (Just(one,NO s))
                           else return (Just(one,ONCE s))

newModes :: Int -> Mode -> Gen [Mode]
newModes n (ONCE s) = do
  n' <- chooseInt (0,n-1)
  return [ if j == n' then (ONCE s) else (NO s) | j <- [0 .. n-1] ]
newModes n m = return (replicate n m)

theSymbol :: Mode -> Maybe String
theSymbol NONE = Nothing
theSymbol (NO s) = Just s
theSymbol (ONCE s) = Just s

