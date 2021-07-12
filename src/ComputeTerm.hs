module ComputeTerm (
   term,
   sameTypeTerms
   )where

import DataType
import GetSignatureInfo (allFunctions,allConstants,theType)
import Data.List ((\\))
import Data.Maybe (fromJust)

-- for #3
-- first parameter is a bound on complexity of terms
term :: Int -> Signature -> [Term]
term n sig = take n (constant ++ subterms n sig constant)
                  where conSymbol = map symbol (allConstants sig)
                        constant = map (`Term` []) conSymbol

sameTypeTerms :: Signature -> [Term] -> Type -> [Term]
sameTypeTerms sig ts t = filter (\x -> t == fromJust(theType sig (termName x))) ts

diffTypeTerms :: Signature -> [Term] -> [Type] -> [[Term]]
diffTypeTerms sig ts = map (sameTypeTerms sig ts)

allTerms :: Signature -> [FunctionSymbol] -> [Term] -> [Term]
allTerms sig fs ts = concatMap (\x -> map (Term (symbol x)) (sequence(diffTypeTerms sig ts (arguments x)))) fs

subterms :: Int -> Signature -> [Term] -> [Term]
subterms n sig w = if null (getAllT \\ w) || length getAllT >= n
                      then getAllT
                      else subterms n sig (w ++ (getAllT \\ w))
                         where func = allFunctions sig
                               getAllT = allTerms sig func w


