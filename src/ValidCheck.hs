module ValidCheck (
   isValid,
   )where

import DataType
import GetSignatureInfo (giveType,getAllConstantSymbol,getSigSymbol)

isValid :: Signature -> Term -> Bool
isValid (Signature xs) t = all (`elem` getSigSymbol xs) (getTermSymbol t) && checkType t xs

giveArgType :: String -> [FunctionSymbol] -> [Type]
giveArgType _ [] = error "This will never happen!"
giveArgType x (FunctionSymbol y ys _:xs)
   | x == y = ys
   | otherwise = giveArgType x xs

checkType :: Term -> [FunctionSymbol] -> Bool
checkType (Term s xs) w = checkType' (giveArgType s w) xs w

checkType' :: [Type] ->[Term] -> [FunctionSymbol] -> Bool
checkType' [] [] _ = True
checkType' [] xs _ = null xs
checkType' xs [] _ = null xs
checkType' (t:ts) (Term s []:xs) w = checkType' ts xs w && giveType s w == Just t && s `elem` getAllConstantSymbol w
checkType' (t:ts) (Term s x':xs) w = checkType (Term s x') w && checkType' ts xs w && giveType s w == Just t

getTermSymbol :: Term -> [String]
getTermSymbol (Term x xs) = x : getTermSymbol' xs

getTermSymbol' :: [Term] -> [String]
getTermSymbol' [] = []
getTermSymbol' (Term x xs:ys) = [x] ++ getTermSymbol' xs ++ getTermSymbol' ys



