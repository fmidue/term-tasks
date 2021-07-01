module ValidCheck (
   isValid
   )where

import Data.Maybe (fromJust)
import DataType
import GetSignatureInfo (giveType,getAllConstantSymbol,getSigSymbol,giveArgType)
import DealWithTerm (getTermSymbol)

isValid :: Signature -> Term -> Bool
isValid sig t = all (`elem` getSigSymbol sig) (getTermSymbol t) && checkType t sig

checkType :: Term -> Signature -> Bool
checkType (Term s xs) w = checkType' (fromJust (giveArgType s w)) xs w

checkType' :: [Type] ->[Term] -> Signature -> Bool
checkType' [] [] _ = True
checkType' [] xs _ = null xs
checkType' xs [] _ = null xs
checkType' (t:ts) (Term s []:xs) w = checkType' ts xs w && giveType s w == Just t && s `elem` getAllConstantSymbol w
checkType' (t:ts) (Term s x':xs) w = checkType (Term s x') w && checkType' ts xs w && giveType s w == Just t



