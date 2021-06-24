module ValidCheck (
   isValid,
   getSigTermName
   )where

import DataType
import ComputeTerm

--hasType :: Type -> Term  -> [FunctionSymbol] -> Bool
--hasType = checkType

isValid :: Signature -> Term -> Bool
isValid (Signature xs) t = checkSymbol d c && checkNumber t a && checkType b t xs
                           where a = giveSymbol t xs
                                 b = giveType t xs
                                 c = getSigName xs
                                 d = getTermName t

checkNumber :: Term -> Maybe FunctionSymbol -> Bool
checkNumber (Term _ []) (Just (FunctionSymbol _ [] _)) = True
checkNumber (Term _ []) _ = False
checkNumber _ Nothing = False
checkNumber (Term _ xs) (Just (FunctionSymbol _ ys _)) = length xs == length ys

giveSymbol :: Term -> [FunctionSymbol] -> Maybe FunctionSymbol
giveSymbol _ [] = Nothing
giveSymbol (Term a ys) (FunctionSymbol b x t : xs)
   | a == b = Just (FunctionSymbol b x t)
   | otherwise = giveSymbol (Term a ys) xs

giveArgType :: Term -> [FunctionSymbol] -> [Type]
giveArgType _ [] = []
giveArgType (Term _ []) _ = []
giveArgType (Term x xs) (FunctionSymbol y ys _:l)
   | x == y = ys
   | otherwise = giveArgType (Term x xs) l

getSigTermName :: [FunctionSymbol] -> [String]
getSigTermName [] = []
getSigTermName (FunctionSymbol s xs _ : ys)
   | null xs = s : getSigTermName ys
   | otherwise = getSigTermName ys

checkType :: Maybe Type -> Term -> [FunctionSymbol] -> Bool
checkType Nothing _ _ = False
checkType _ (Term s xs) w = a
   where a = checkType' b xs w
         b = giveArgType (Term s xs) w

checkType' :: [Type] ->[Term] -> [FunctionSymbol] -> Bool
checkType' [] [] _ = True
checkType' [] xs _ = null xs
checkType' xs [] _ = null xs
checkType' (t:ts) (Term a []:xs) w = checkType' ts xs w && giveType (Term a []) w == Just t && a `elem` b
   where b = getSigTermName w
checkType' (t:ts) (Term a x':xs) w = checkType' n x' w && checkType' ts xs w && giveType (Term a x') w == Just t
   where n = giveArgType (Term a x') w

getSigName :: [FunctionSymbol] -> [String]
getSigName [] = []
getSigName (FunctionSymbol x _ _:ys) = x : getSigName ys

getTermName :: Term -> [String]
getTermName (Term x xs) = x : getTermName' xs

getTermName' :: [Term] -> [String]
getTermName' [] = []
getTermName' (Term x xs:ys) = [x] ++ getTermName' xs ++ getTermName' ys

checkSymbol :: [String] -> [String] -> Bool
checkSymbol [] _ = True
checkSymbol (x:xs) ys = x `elem` ys && checkSymbol xs ys

