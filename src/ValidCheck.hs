module ValidCheck (
   hasType,
   isValid,
   checkNumber,
   giveSymbol,
   make,
   checkType,
   checkType',
   giveType,
   getSigName,
   getTermName,
   getTermName',
   checkSymbol
   )where

import DataType

hasType :: Type -> Term  -> [FunctionSymbol] -> Bool
hasType = checkType 

isValid :: Signature -> Term -> Bool
isValid (Signature xs) t = checkNumber t a && checkType b t xs && checkSymbol d c
                           where a = giveSymbol t xs
                                 b = giveType t xs
                                 c = getSigName xs
                                 d = getTermName t

checkNumber :: Term -> FunctionSymbol -> Bool
checkNumber (Term _) _ = True
checkNumber (Function _ xs) (FunctionSymbol _ ys _) = length xs == length ys

giveSymbol :: Term -> [FunctionSymbol] -> FunctionSymbol
giveSymbol _ [] = FunctionSymbol "e" [] (Type "error")
giveSymbol (Term a) (FunctionSymbol b x t : xs)
   | a == b = FunctionSymbol b x t
   | otherwise = giveSymbol (Term a) xs
giveSymbol (Function a ys) (FunctionSymbol b x t : xs)
   | a == b = FunctionSymbol b x t
   | otherwise = giveSymbol (Function a ys) xs

make :: Term -> [FunctionSymbol] -> [Type]
make _ [] = []
make (Term _) _ = []
make (Function x xs) (FunctionSymbol y ys _:l) 
   | x == y = ys 
   | otherwise = make (Function x xs) l

checkType :: Type -> Term -> [FunctionSymbol] -> Bool
checkType (Type "error") _ _ = False
checkType t (Term a) w = giveType (Term a) w == t 
checkType t (Function s xs) w = a && b
   where a = giveType (Function s xs) w == t
         b = checkType' c xs w
         c = make (Function s xs) w

checkType' :: [Type] ->[Term] -> [FunctionSymbol] -> Bool
checkType' [] [] _ = True
checkType' [] xs _ = null xs
checkType' xs [] _ = null xs
checkType' (t:ts) (Term a:xs) w = checkType' ts xs w && giveType (Term a) w == t
checkType' (t:ts) (Function a x':xs) w = checkType' n x' w && checkType' ts xs w && giveType (Function a x') w == t
   where n = make (Function a x') w 

giveType :: Term -> [FunctionSymbol] -> Type
giveType (Term _) [] = (Type "error")
giveType (Term a) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Term a) ys
giveType (Function _ _) [] = (Type "error")
giveType (Function a s) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Function a s) ys

getSigName :: [FunctionSymbol] -> [String]
getSigName [] = []
getSigName (FunctionSymbol x _ _:ys) = x : getSigName ys

getTermName :: Term -> [String]
getTermName (Term x) = [x]
getTermName (Function x' xs) = [x'] ++ getTermName' xs

getTermName' :: [Term] -> [String]
getTermName' [] = []
getTermName' (Term x:ys) = [x] ++ getTermName' ys
getTermName' (Function x' xs:ys) = [x'] ++ getTermName' xs ++ getTermName' ys 

checkSymbol :: [String] -> [String] -> Bool
checkSymbol [] _ = True
checkSymbol (x:xs) ys = x `elem` ys && checkSymbol xs ys
