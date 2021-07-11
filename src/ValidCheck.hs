module ValidCheck (
   isValid
   )where

import Data.Maybe (fromJust)
import DataType
import GetSignatureInfo (giveType,getAllConstant,getSigSymbol,giveArgType)
import DealWithTerm (getTermSymbol)

isValid :: Signature -> Term -> Bool
isValid sig t = all (`elem` getSigSymbol sig) (getTermSymbol t) && checkType t sig

checkType :: Term -> Signature -> Bool
checkType (Term s xs) w = checkType' (fromJust (giveArgType w s)) xs w

checkType' :: [Type] ->[Term] -> Signature -> Bool
checkType' [] [] _ = True
checkType' [] xs _ = null xs
checkType' xs [] _ = null xs
checkType' (t:ts) (Term s []:xs) w = checkType' ts xs w && giveType w s == Just t && s `elem` map funcName (getAllConstant w)
checkType' (t:ts) (Term s x':xs) w = checkType (Term s x') w && checkType' ts xs w && giveType w s == Just t



