module OneTerm (
  oneValidTerm
  )where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType
import Data.List ((\\))
import Data.Maybe (catMaybes)

oneValidTerm :: Signature -> Int -> Int -> Gen (Maybe Term)
oneValidTerm sig@(Signature fs) a b = arbTerm sig a b fs

arbTerm :: Signature -> Int -> Int -> [FunctionSymbol] -> Gen (Maybe Term)
arbTerm _ _ _ [] = return Nothing
arbTerm sig a b fs= do
  one <- elements fs
  termList <- mapM ((arbTerm sig a b . allSameTypes sig)) (arguments one)
  let termList' = catMaybes termList
      nList = map number termList'
      n = sum nList + 1
  if (n <= b)
  then return (Just (Term (symbol one) termList'))
  else arbTerm sig a b (fs\\[one])

number :: Term -> Int
number fs = 1 + number' (parameters fs)

number' :: [Term] -> Int
number' [] = 0
number' (f:fs)
  | null (parameters f) = 1 + number' fs
  | otherwise = 1 + number' (parameters f) + number' fs





