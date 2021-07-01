module ValidCheck (
   isValid,
   checkErrorType
   )where

import Data.Maybe (fromJust)
import Data.List ((\\))
import DataType
import GetSignatureInfo (giveType,getAllConstantSymbol,getSigSymbol,giveArgType,giveTypeList)
import DealWithTerm (getTermSymbol,getTermSymbol')

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

checkErrorType :: Signature -> Term -> Maybe Error
checkErrorType xs t
   | isValid xs t = Nothing
   | lengthError xs t = Just LENGTH
   | typeError xs t = Just TYPE
   | orderError xs t = Just ORDER
   | otherwise = Nothing

lengthError :: Signature -> Term -> Bool
lengthError xs (Term s ys) = length ys /= length (fromJust(giveArgType s xs))

orderError :: Signature -> Term -> Bool
orderError xs (Term s ys) = compareType (giveTypeList (getTermSymbol' ys) xs) (fromJust (giveArgType s xs))

compareType :: [Type] -> [Type] -> Bool
compareType [] [] = True
compareType [] _ = False
compareType (t:ts) xs
   | t `elem` xs = compareType ts (xs\\[t])
   | otherwise = compareType ts xs

typeError :: Signature -> Term -> Bool
typeError xs (Term s ys) = not (all (`elem` ts) ts') || not (all (`elem` ts') ts)
                              where ts = giveTypeList (getTermSymbol' ys) xs
                                    ts' = fromJust (giveArgType s xs)


