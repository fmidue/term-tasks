module GetSignatureInfo (
   getSigSymbol,
   getAllConstant,
   getAllFunction,
   getAllType,
   giveType,
   giveArgType,
   getAllSameType
)
where

import DataType
import Data.List (nub)

getSigSymbol :: Signature -> [String]
getSigSymbol (Signature []) = []
getSigSymbol (Signature (FunctionSymbol x _ _:ys)) = x : getSigSymbol (Signature ys)

getAllFunction :: Signature -> [FunctionSymbol]
getAllFunction (Signature []) = []
getAllFunction (Signature(FunctionSymbol s xs t:ys))
   | null xs = getAllFunction (Signature ys)
   | otherwise = FunctionSymbol s xs t : getAllFunction (Signature ys)

getAllConstant :: Signature -> [FunctionSymbol]
getAllConstant (Signature []) = []
getAllConstant (Signature (FunctionSymbol s ys t : xs))
    | null ys = FunctionSymbol s ys t : getAllConstant (Signature xs)
    | otherwise = getAllConstant (Signature xs)

getAllType :: Signature -> [Type]
getAllType (Signature []) = []
getAllType (Signature (FunctionSymbol _ x y : xs)) = nub (x++y:getAllType (Signature xs))

giveType :: Signature -> String -> Maybe Type
giveType (Signature []) _ = Nothing
giveType (Signature(FunctionSymbol b _ t : ys)) a
   | a == b = Just t
   | otherwise =  giveType (Signature ys) a

giveArgType :: String -> Signature -> Maybe [Type]
giveArgType _ (Signature []) = Nothing
giveArgType x (Signature(FunctionSymbol y ys _:xs))
   | x == y = Just ys
   | otherwise = giveArgType x (Signature xs)

getAllSameType :: Signature -> Type -> [FunctionSymbol]
getAllSameType (Signature []) _ = []
getAllSameType (Signature (FunctionSymbol s xs t' : ys)) t
   | t == t' = FunctionSymbol s xs t' : getAllSameType (Signature ys) t
   | otherwise = getAllSameType (Signature ys) t


