module Examples.ValidCheck where

import DataType
import Examples.Functions (theArgumentsTypes,allConstants,theType,termSymbols)
import Data.Maybe (fromJust)

isValid :: Signature -> Term -> Bool
isValid sig t = all (`elem` allSymbols sig) (termSymbols t) && isValidType t sig

isValidType :: Term -> Signature -> Bool
isValidType (Term s xs) w = isValidType' (fromJust (theArgumentsTypes w s)) xs w

isValidType' :: [Type] ->[Term] -> Signature -> Bool
isValidType' [] [] _ = True
isValidType' [] xs _ = null xs
isValidType' xs [] _ = null xs
isValidType' (t:ts) (Term s []:xs) w = isValidType' ts xs w && theType w s == Just t && s `elem` map #symbol (allConstants w)
isValidType' (t:ts) (Term s x':xs) w = isValidType (Term s x') w && isValidType' ts xs w && theType w s == Just t