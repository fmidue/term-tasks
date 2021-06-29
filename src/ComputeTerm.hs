module ComputeTerm (
   term,
   termsOfType,
   printTerm,
   printTermsOfType,
   )where

import DataType
import GetSignatureInfo (getAllFunction,getAllConstantSymbol,giveType)
import DealWithTerm (transTerm)
import Data.List ((\\),nub)

-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n sig = take n (nub (constant ++ makeSubterm n sig ft))
                  where conSymbol = getAllConstantSymbol sig
                        constant = makeConstants conSymbol
                        ft = constant ++ makeAllTerm sig (getAllFunction sig) constant

-- maybe also for terms of a specific type
termsOfType :: Int -> Type -> Signature -> [Term]
termsOfType n t sig = if null (term n sig)
                      then []
                      else getSameTypeTerm t (term n sig) sig

-- can tansform [Term] in more readable forms
printTerm :: Int -> Signature -> [String]
printTerm n xs = nub (map transTerm (term n xs))

printTermsOfType :: Int -> Type -> Signature -> [String]
printTermsOfType n t xs = nub (map transTerm (termsOfType n t xs))

makeConstants :: [String] -> [Term]
makeConstants = map (`Term` [])

getSameTypeTerm :: Type -> [Term] -> Signature -> [Term]
getSameTypeTerm _ [] _ = []
getSameTypeTerm a (Term s t:xs) sig
   | Just a == giveType s sig = Term s t : getSameTypeTerm a xs sig
   | otherwise = getSameTypeTerm a xs sig

makeDiffTypeTermList :: Signature -> [Term] -> FunctionSymbol -> [[Term]]
makeDiffTypeTermList _ _ (FunctionSymbol _ [] _) = []
makeDiffTypeTermList w s (FunctionSymbol a (x:xs) y) = getSameTypeTerm x s w : makeDiffTypeTermList w s (FunctionSymbol a xs y)

makeTerm :: String -> [[Term]] -> [Term]
makeTerm _ [] = []
makeTerm s (x:xs) = Term s x : makeTerm s xs

makeAllTerm :: Signature -> [FunctionSymbol] -> [Term] -> [Term]
makeAllTerm _ [] _ = []
makeAllTerm w (FunctionSymbol a x y : xs) t = makeTerm a (sequence(makeDiffTypeTermList w t (FunctionSymbol a x y))) ++ makeAllTerm w xs t

makeSubterm :: Int -> Signature -> [Term] -> [Term]
makeSubterm n sig w = if null (nub getAllT \\ w) || length (nub getAllT) >= n
                      then nub (makeAllTerm sig subTerm w)
                      else makeSubterm n sig (w ++ (nub getAllT \\ w))
                         where subTerm = getAllFunction sig
                               getAllT = makeAllTerm sig subTerm w


