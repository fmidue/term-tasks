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

tr = [FunctionSymbol "x" [] A,FunctionSymbol "y" [] B,FunctionSymbol "z" [] C,FunctionSymbol "f" [A,A] B,FunctionSymbol "g" [A,B] C,FunctionSymbol "h" [A,B,C] D]

hasType :: Term -> Type -> Bool
hasType x t = checkType t x tr

isValid :: Signature -> Term -> Bool
isValid (Signature xs) t = checkNumber t a && checkType b t xs && checkSymbol d c
                           where a = giveSymbol t xs
                                 b = giveType t xs
                                 c = getSigName xs
                                 d = getTermName t

checkNumber :: Term -> FunctionSymbol -> Bool
checkNumber (Term _ xs) (FunctionSymbol _ ys _) = length xs == length ys
checkNumber (Function _ xs) (FunctionSymbol _ ys _) = length xs == length ys

giveSymbol :: Term -> [FunctionSymbol] -> FunctionSymbol
giveSymbol _ [] = FunctionSymbol "o" [] O
giveSymbol (Term a ys) (FunctionSymbol b x t : xs)
   | a == b = FunctionSymbol b x t
   | otherwise = giveSymbol (Term a ys) xs
giveSymbol (Function a ys) (FunctionSymbol b x t : xs)
   | a == b = FunctionSymbol b x t
   | otherwise = giveSymbol (Function a ys) xs

make :: [Term] -> [FunctionSymbol] -> [Type]
make [] _ = []
make (x:xs) w = giveType x w : make xs w

checkType :: Type -> Term -> [FunctionSymbol] -> Bool
checkType O _ _ = False
checkType t (Term a x) w = giveType (Term a x) w == t 
checkType t (Function s xs) w = a && b
   where a = giveType (Function s xs) w == t
         b = checkType' c xs w
         c = make xs w

checkType' :: [Type] ->[Term] -> [FunctionSymbol] -> Bool
checkType' _ [] _ = True
checkType' (t:ts) (Term a x:xs) w = checkType' ts xs w && giveType (Term a x) w == t
checkType' (t:ts) (Function a x':xs) w = checkType' n x' w && checkType' ts xs w && giveType (Function a x') w == t
   where n = make x' w 

giveType :: Term -> [FunctionSymbol] -> Type
giveType (Term _ _) [] = O
giveType (Term a s) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Term a s) ys
giveType (Function _ _) [] = O
giveType (Function a s) (FunctionSymbol b _ t : ys)
   | a == b = t 
   | otherwise =  giveType (Function a s) ys

getSigName :: [FunctionSymbol] -> [String]
getSigName [] = []
getSigName (FunctionSymbol x _ _:ys) = x : getSigName ys

getTermName :: Term -> [String]
getTermName (Term x _) = [x]
getTermName (Function x' xs) = [x'] ++ getTermName' xs

getTermName' :: [Term] -> [String]
getTermName' [] = []
getTermName' (Term x _:ys) = [x] ++ getTermName' ys
getTermName' (Function x' xs:ys) = [x'] ++ getTermName' xs ++ getTermName' ys 

checkSymbol :: [String] -> [String] -> Bool
checkSymbol [] _ = True
checkSymbol (x:xs) ys = x `elem` ys && checkSymbol xs ys
