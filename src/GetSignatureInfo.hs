module GetSignatureInfo (
   getSigSymbol,
   getFuncSymbol,
   getAllFunction,
   getAllConstantSymbol,
   getAllType,
   giveType
)
where

import DataType
import Data.List (nub)

getSigSymbol :: [FunctionSymbol] -> [String]
getSigSymbol [] = []
getSigSymbol (FunctionSymbol x _ _:ys) = x : getSigSymbol ys

getFuncSymbol :: Signature -> [String]
getFuncSymbol (Signature []) = []
getFuncSymbol (Signature (FunctionSymbol s ys _ : xs))
    | null ys = getFuncSymbol (Signature xs)
    | otherwise = s : getFuncSymbol (Signature xs)

getAllFunction :: [FunctionSymbol] -> [FunctionSymbol]
getAllFunction [] = []
getAllFunction (FunctionSymbol s xs t:ys)
   | null xs = getAllFunction ys
   | otherwise = FunctionSymbol s xs t : getAllFunction ys

getAllConstantSymbol :: [FunctionSymbol] -> [String]
getAllConstantSymbol [] = []
getAllConstantSymbol (FunctionSymbol s xs _ : ys)
   | null xs = s : getAllConstantSymbol ys
   | otherwise = getAllConstantSymbol ys

getAllType :: Signature -> [Type]
getAllType (Signature []) = []
getAllType (Signature (FunctionSymbol _ x y : xs)) = nub (x++y:getAllType (Signature xs))

giveType :: String -> [FunctionSymbol] -> Maybe Type
giveType _ [] = Nothing
giveType a (FunctionSymbol b _ t : ys)
   | a == b = Just t
   | otherwise =  giveType a ys



