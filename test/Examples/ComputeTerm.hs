module Examples.ComputeTerm where

import DataType
import Examples.Functions (allConstants,theType,allFunctions)
import Data.Maybe (fromJust)
import Data.List ((\\))

term :: Int -> Signature -> [Term String]
term n sig = take n (constant ++ subTerms n sig constant)
                  where conSymbol = map #symbol (allConstants sig)
                        constant = map (`Term` []) conSymbol

sameTypeTerms :: Signature -> [Term String] -> Type -> [Term String]
sameTypeTerms sig ts t = filter (\x -> t == fromJust(theType sig (#symbol x))) ts

diffTypeTerms :: Signature -> [Term String] -> [Type] -> [[Term String]]
diffTypeTerms sig ts = map (sameTypeTerms sig ts)

theTerms :: Signature -> [Symbol] -> [Term String] -> [Term String]
theTerms sig fs ts = concatMap (\x -> map (Term (#symbol x)) (sequence(diffTypeTerms sig ts (#arguments x)))) fs

subTerms :: Int -> Signature -> [Term String] -> [Term String]
subTerms n sig w = if null (getAllT \\ w) || length getAllT >= n
                      then getAllT
                      else subTerms n sig (w ++ (getAllT \\ w))
                         where func = allFunctions sig
                               getAllT = theTerms sig func w