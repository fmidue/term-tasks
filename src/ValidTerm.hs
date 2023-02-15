module ValidTerm (
  validTerms,
  oneValidTerm
)where

import Test.QuickCheck (Gen, elements)
import DataType (Signature(..), Symbol(..), Term(..), allSameTypes, Type)
import Control.Monad (MonadPlus, guard, msum)
import Data.List (nub)

data Mode = None | No String | Once String

validTerms :: Signature -> Maybe String -> Int -> Int -> Maybe [Type] -> [Term]
validTerms sig symbolOnce a b = nub . arbTerms sig (maybe None Once symbolOnce) a b

arbTerms :: MonadPlus l => Signature -> Mode -> Int -> Int -> Maybe [Type] -> l Term
arbTerms sig m a b root = do
  let fs = maybe (#definitions sig) (concatMap $ allSameTypes sig) root
  (one,newMode) <- symbolWithModes m fs
  guard (not (isConstant one && isOnce newMode))
  n' <- msum $ map return $ division (length (#arguments one)) a b
  modeList <- msum $ map return $ newModes (length (#arguments one)) newMode
  termList <- mapM (\(ml,t,(a',b')) -> arbTerms sig ml a' b' (Just [t])) (zip3 modeList (#arguments one) n')
  return (Term (#symbol one) termList)

oneValidTerm :: Signature -> Maybe String -> Int -> Int -> Maybe [Type] -> Gen (Maybe Term)
oneValidTerm sig symbolOnce a b root = do
  let terms = validTerms sig symbolOnce a b root
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
symbolWithModes None fs = msum [return (one, None) | one <- fs]
symbolWithModes (No s) fs = msum [return (one, No s) | one <- fs, #symbol one /= s]
symbolWithModes (Once s) fs = msum [return $ if #symbol one == s then (one, No s) else (one, Once s) | one <- fs]

newModes :: Int -> Mode -> [[Mode]]
newModes 0 _ = [[]]
newModes n (Once s) = [ [ if j == n' then Once s else No s | j <- [0 .. n-1] ] | n' <- [0 .. n-1] ]
newModes n m = [replicate n m]

isConstant :: Symbol -> Bool
isConstant (Symbol _ xs _) = null xs

isOnce :: Mode -> Bool
isOnce (Once _) = True
isOnce _ = False
