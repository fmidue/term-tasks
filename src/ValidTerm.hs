module ValidTerm (
  validTerms,
  oneValidTerm
)where

import Test.QuickCheck (Gen, elements)
import DataType (Signature(..),Symbol(..),Term(..),allSameTypes)
import Data.List (transpose,nub)
import Control.Monad (replicateM)

data Mode = NONE | NO String | ONCE String

validTerms :: Signature -> Maybe String -> Int -> Int -> [Term]
validTerms sig@(Signature fs) Nothing a b = nub(arbTerms sig NONE a b fs)
validTerms sig@(Signature fs) (Just s) a b = nub(arbTerms sig (ONCE s) a b fs)

arbTerms :: Signature -> Mode -> Int -> Int -> [Symbol] -> [Term]
arbTerms sig m a b fs = [Term (#symbol one) termList |
                         (one,newMode) <- symbolWithModes m fs,
                         not (isConstant one && isOnce newMode),
                         n' <- division (length (#arguments one)) a b,
                         modeList <- newModes (length (#arguments one)) newMode,
                         termList <- mapM (\(ml,t,(a',b')) -> arbTerms sig ml a' b' . allSameTypes sig $t) (zip3 modeList (#arguments one) n')]

oneValidTerm :: Signature -> Maybe String -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig s a b = do
  let terms = validTerms sig s a b
  case terms of
    [] -> return Nothing
    ts -> do
      term <- elements ts
      return (Just term)

division ::Int -> Int -> Int -> [[(Int,Int)]]
division n a b
  | a > b  = []
  | n == 0 && a == 1 = [[]]
  | n >= a = division n (n + 1) b
  | n == 0 = []
  | otherwise = recursively n (a - 1) (b - 1)
  where
    recursively :: Int -> Int -> Int -> [[(Int,Int)]]
    recursively 1 a b = [[(a,b)]]
    recursively n a b
      = [ (i,j) : ds | i <- [1 .. a - n + 1], j <- [i .. i + b - a], ds <- recursively (n - 1) (a - i) (b - j) ]

symbolWithModes :: Mode -> [Symbol] -> [(Symbol,Mode)]
symbolWithModes NONE fs = [(one,NONE) | one <- fs]
symbolWithModes (NO s) fs = [(one,NO s) | one <- fs, #symbol one /= s]
symbolWithModes (ONCE s) fs = [if #symbol one == s then (one,NO s) else (one,ONCE s) | one <- fs]

newModes :: Int -> Mode -> [[Mode]]
newModes 0 _ = [[]]
newModes n (ONCE s) = [[if j == n' then ONCE s else NO s | j <- [0 .. n-1]]|n' <- [0..n-1]]
newModes n m = [replicate n m]

isConstant :: Symbol -> Bool
isConstant (Symbol _ xs _) = null xs

isOnce :: Mode -> Bool
isOnce (ONCE _) = True
isOnce _ = False
