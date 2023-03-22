module Examples.ValidCheck where

import DataType
import Examples.Functions (theArgumentsTypes,allConstants,theType,termSymbols)
import Data.Maybe (fromJust)

isValid :: Signature -> Term String -> Bool
isValid sig t = all (`elem` allSymbols sig) (termSymbols t) && isValidType t sig

isValidType :: Term String -> Signature -> Bool
isValidType (Term s xs) w = isValidType' (fromJust (theArgumentsTypes w s)) xs w

isValidType' :: [Type] ->[Term String] -> Signature -> Bool
isValidType' [] [] _ = True
isValidType' [] _  _ = False
isValidType' _  [] _ = False
isValidType' (t:ts) (Term s []:xs) w = isValidType' ts xs w && theType w s == Just t && s `elem` map #symbol (allConstants w)
isValidType' (t:ts) (Term s x':xs) w = isValidType (Term s x') w && isValidType' ts xs w && theType w s == Just t
