module ComputeTerm (
   term,
   getSameTypeTerm,
   makeConstants,
   makeSubterm,
   oneValidTerm
   )where

import DataType
import GetSignatureInfo (getAllFunction,getAllConstantSymbol,giveType)
import Data.List ((\\))
import Test.QuickCheck (Gen,elements,chooseInt)

-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n sig = take n (constant ++ makeSubterm n sig constant)
                  where conSymbol = getAllConstantSymbol sig
                        constant = makeConstants conSymbol

makeConstants :: [String] -> [Term]
makeConstants = map (`Term` [])

getSameTypeTerm :: Type -> [Term] -> Signature -> [Term]
getSameTypeTerm _ [] _ = []
getSameTypeTerm a (Term s t:xs) sig
   | Just a == giveType s sig = Term s t : getSameTypeTerm a xs sig
   | otherwise = getSameTypeTerm a xs sig

makeDiffTypeTermList :: Signature -> [Term] -> [Type] -> [[Term]]
makeDiffTypeTermList _ _ [] = []
makeDiffTypeTermList w s (x:xs) = getSameTypeTerm x s w : makeDiffTypeTermList w s xs

makeTerm :: String -> [[Term]] -> [Term]
makeTerm s = map (Term s)

makeAllTerm :: Signature -> [FunctionSymbol] -> [Term] -> [Term]
makeAllTerm _ [] _ = []
makeAllTerm w (FunctionSymbol a x _ : xs) t = makeTerm a (sequence(makeDiffTypeTermList w t x)) ++ makeAllTerm w xs t

makeSubterm :: Int -> Signature -> [Term] -> [Term]
makeSubterm n sig w = if null (getAllT \\ w) || length getAllT >= n
                      then getAllT
                      else makeSubterm n sig (w ++ (getAllT \\ w))
                         where func = getAllFunction sig
                               getAllT = makeAllTerm sig func w

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig = do
   n <- chooseInt (1,20)
   let conSymbol = getAllConstantSymbol sig
       constant = makeConstants conSymbol
       getAllT = makeSubterm n sig constant
       allTerm = constant ++ getAllT
   elements allTerm

