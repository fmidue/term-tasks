module ValidCheck (
   hasType,
   isValid,
   checkNumber,
   giveSymbol,
   make,
   getSigTermName,
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
checkNumber (Term _ []) _ = True
checkNumber (Term _ xs) (FunctionSymbol _ ys _) = length xs == length ys

giveSymbol :: Term -> [FunctionSymbol] -> FunctionSymbol
giveSymbol _ [] = FunctionSymbol "e" [] (Type "error")
giveSymbol (Term a ys) (FunctionSymbol b x t : xs)
   | a == b = FunctionSymbol b x t
   | otherwise = giveSymbol (Term a ys) xs

make :: Term -> [FunctionSymbol] -> [Type]
make _ [] = []
make (Term _ []) _ = []
make (Term x xs) (FunctionSymbol y ys _:l)
   | x == y = ys
   | otherwise = make (Term x xs) l

getSigTermName :: [FunctionSymbol] -> [String]
getSigTermName [] = []
getSigTermName (FunctionSymbol s xs _ : ys)
   | null xs = s : getSigTermName ys
   | otherwise = getSigTermName ys

checkType :: Type -> Term -> [FunctionSymbol] -> Bool
checkType (Type "error") _ _ = False
--checkType t (Term a []) w = giveType (Term a []) w == t && a `elem` n
--   where n = getSigTermName w
checkType _ (Term s xs) w = a
   where a = checkType' b xs w
         b = make (Term s xs) w

checkType' :: [Type] ->[Term] -> [FunctionSymbol] -> Bool
checkType' [] [] _ = True
checkType' [] xs _ = null xs
checkType' xs [] _ = null xs
checkType' (t:ts) (Term a []:xs) w = checkType' ts xs w && giveType (Term a []) w == t && a `elem` b
   where b = getSigTermName w
checkType' (t:ts) (Term a x':xs) w = checkType' n x' w && checkType' ts xs w && giveType (Term a x') w == t
   where n = make (Term a x') w

giveType :: Term -> [FunctionSymbol] -> Type
giveType (Term _ _) [] = (Type "error")
giveType (Term a s) (FunctionSymbol b _ t : ys)
   | a == b = t
   | otherwise =  giveType (Term a s) ys

getSigName :: [FunctionSymbol] -> [String]
getSigName [] = []
getSigName (FunctionSymbol x _ _:ys) = x : getSigName ys

getTermName :: Term -> [String]
getTermName (Term x xs) = [x] ++ getTermName' xs

getTermName' :: [Term] -> [String]
getTermName' [] = []
getTermName' (Term x xs:ys) = [x] ++ getTermName' xs ++ getTermName' ys

checkSymbol :: [String] -> [String] -> Bool
checkSymbol [] _ = True
checkSymbol (x:xs) ys = x `elem` ys && checkSymbol xs ys

