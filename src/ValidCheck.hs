module ValidCheck (
   isValid
   )where

import Data.Maybe (fromJust)
import DataType
import GetSignatureInfo (theType,allConstants,allSymbols,theArgumentsTypes)
import DealWithTerm (termSymbols)

isValid :: Signature -> Term -> Bool
isValid sig t = all (`elem` allSymbols sig) (termSymbols t) && validType t sig

validType :: Term -> Signature -> Bool
validType (Term s xs) w = validType' (fromJust (theArgumentsTypes w s)) xs w

validType' :: [Type] ->[Term] -> Signature -> Bool
validType' [] [] _ = True
validType' [] xs _ = null xs
validType' xs [] _ = null xs
validType' (t:ts) (Term s []:xs) w = validType' ts xs w && theType w s == Just t && s `elem` map symbol (allConstants w)
validType' (t:ts) (Term s x':xs) w = validType (Term s x') w && validType' ts xs w && theType w s == Just t



