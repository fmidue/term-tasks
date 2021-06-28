module ValidCheck (
   isValid,
   )where

import DataType
import ComputeTerm (giveType,getAllConstant)

--hasType :: Type -> Term  -> [FunctionSymbol] -> Bool
--hasType = checkType

isValid :: Signature -> Term -> Bool
isValid (Signature xs) t@(Term root ys) = all (`elem` getSigSymbol xs) (getTermSymbol t) && checkNumber ys (giveSymbol root xs) && checkType t xs

checkNumber :: [Term] -> Maybe FunctionSymbol -> Bool
checkNumber [] (Just (FunctionSymbol _ [] _)) = True
checkNumber [] _ = False
checkNumber _ Nothing = False
checkNumber xs (Just (FunctionSymbol _ ys _)) = length xs == length ys

giveSymbol :: String -> [FunctionSymbol] -> Maybe FunctionSymbol
giveSymbol _ [] = Nothing
giveSymbol s (FunctionSymbol b x t : xs)
   | s == b = Just (FunctionSymbol b x t)
   | otherwise = giveSymbol s xs

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
checkType' (t:ts) (Term s []:xs) w = checkType' ts xs w && giveType s w == Just t && s `elem` getAllConstant w
checkType' (t:ts) (Term s x':xs) w = checkType' (giveArgType s w) x' w && checkType' ts xs w && giveType s w == Just t

getSigSymbol :: [FunctionSymbol] -> [String]
getSigSymbol [] = []
getSigSymbol (FunctionSymbol x _ _:ys) = x : getSigSymbol ys

getTermSymbol :: Term -> [String]
getTermSymbol (Term x xs) = x : getTermSymbol' xs

getTermSymbol' :: [Term] -> [String]
getTermSymbol' [] = []
getTermSymbol' (Term x xs:ys) = [x] ++ getTermSymbol' xs ++ getTermSymbol' ys



