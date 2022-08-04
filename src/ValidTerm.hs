module ValidTerm (
  validTerms,
  oneValidTerm
)where

import Test.QuickCheck (Gen, elements)
import DataType (Signature(..),Symbol(..),Term(..),allSameTypes)
import Control.Monad (MonadPlus, guard, msum)
import Data.List (nub)

data Mode = NONE | NO String | ONCE String

validTerms :: Signature -> Maybe String -> Int -> Int -> [Term]
validTerms sig@(Signature fs) Nothing a b = nub(arbTerms sig NONE a b fs)
validTerms sig@(Signature fs) (Just s) a b = nub(arbTerms sig (ONCE s) a b fs)

arbTerms :: MonadPlus l => Signature -> Mode -> Int -> Int -> [Symbol] -> l Term
arbTerms sig m a b fs = do
  (one,newMode) <- symbolWithModes m fs
  guard (not (isConstant one && isOnce newMode))
  n' <- msum $ map return $ division (length (#arguments one)) a b
  modeList <- msum $ map return $ newModes (length (#arguments one)) newMode
  termList <- mapM (\(ml,t,(a',b')) -> arbTerms sig ml a' b' . allSameTypes sig $ t) (zip3 modeList (#arguments one) n')
  return (Term (#symbol one) termList)

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
    recursively 1 u v = [[(u,v)]]
    recursively m u v
      = [ (i,j) : ds | i <- [1 .. u - m + 1], j <- [i .. i + v - u], ds <- recursively (m - 1) (u - i) (v - j) ]

symbolWithModes :: MonadPlus l => Mode -> [Symbol] -> l (Symbol,Mode)
symbolWithModes NONE fs = msum [return (one,NONE) | one <- fs]
symbolWithModes (NO s) fs = msum [return (one,NO s) | one <- fs, #symbol one /= s]
symbolWithModes (ONCE s) fs = msum [return $ if #symbol one == s then (one,NO s) else (one,ONCE s) | one <- fs]

newModes :: Int -> Mode -> [[Mode]]
newModes 0 _ = [[]]
newModes n (ONCE s) = [[if j == n' then ONCE s else NO s | j <- [0 .. n-1]]|n' <- [0..n-1]]
newModes n m = [replicate n m]

isConstant :: Symbol -> Bool
isConstant (Symbol _ xs _) = null xs

isOnce :: Mode -> Bool
isOnce (ONCE _) = True
isOnce _ = False
