module ComputeTerm (
   term,
   getSameTypeTerm
   )where

import DataType
import GetSignatureInfo (getAllFunction,getAllConstant,giveType)
import Data.List ((\\))
import Data.Maybe (fromJust)

-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n sig = take n (constant ++ makeSubterm n sig constant)
                  where conSymbol = map funcName (getAllConstant sig)
                        constant = makeConstants conSymbol

makeConstants :: [String] -> [Term]
makeConstants = map (`Term` [])

getSameTypeTerm :: Signature -> [Term] -> Type -> [Term]
getSameTypeTerm sig ts t = filter (\x -> t == fromJust(giveType sig (termName x))) ts

makeDiffTypeTermList :: Signature -> [Term] -> [Type] -> [[Term]]
makeDiffTypeTermList sig ts tList = map (getSameTypeTerm sig ts) tList

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


