module GetSignatureInfo (
   getSigSymbol,
   getFuncSymbol,
   getAllConstant,
   getAllFunction,
   getAllConstantSymbol,
   getAllType,
   giveType,
   giveArgType,
   funcSymbolOfType
)
where

import DataType
import Data.List (nub)

getSigSymbol :: Signature -> [String]
getSigSymbol (Signature []) = []
getSigSymbol (Signature (FunctionSymbol x _ _:ys)) = x : getSigSymbol (Signature ys)

getFuncSymbol :: Signature -> [String]
getFuncSymbol (Signature []) = []
getFuncSymbol (Signature (FunctionSymbol s ys _ : xs))
    | null ys = getFuncSymbol (Signature xs)
    | otherwise = s : getFuncSymbol (Signature xs)

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

getAllConstantSymbol :: Signature -> [String]
getAllConstantSymbol (Signature []) = []
getAllConstantSymbol (Signature(FunctionSymbol s xs _ : ys))
   | null xs = s : getAllConstantSymbol (Signature ys)
   | otherwise = getAllConstantSymbol (Signature ys)

getAllType :: Signature -> [Type]
getAllType (Signature []) = []
getAllType (Signature (FunctionSymbol _ x y : xs)) = nub (x++y:getAllType (Signature xs))

giveType :: String -> Signature -> Maybe Type
giveType _ (Signature []) = Nothing
giveType a (Signature(FunctionSymbol b _ t : ys))
   | a == b = Just t
   | otherwise =  giveType a (Signature ys)

giveArgType :: String -> Signature -> Maybe [Type]
giveArgType _ (Signature []) = Nothing
giveArgType x (Signature(FunctionSymbol y ys _:xs))
   | x == y = Just ys
   | otherwise = giveArgType x (Signature xs)

funcSymbolOfType :: Type -> Signature -> [FunctionSymbol]
funcSymbolOfType _ (Signature []) = []
funcSymbolOfType t (Signature (FunctionSymbol s xs t' : ys))
   | t == t' && not (null xs)= FunctionSymbol s xs t' : funcSymbolOfType t (Signature ys)
   | otherwise = funcSymbolOfType t (Signature ys)
