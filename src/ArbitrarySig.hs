module ArbitrarySig where
import DataType
import Test.QuickCheck
import Data.List (nub)

diffOrder :: Signature -> Gen Signature
diffOrder (Signature fs) = do
    let available = filter (\x -> length (nub (#arguments x)) >=2) fs
    one <- elements available
    (a,b) <- twoDiffTypes (#arguments one)
    let newArg = swap a b (#arguments one)
        newFs = FunctionSymbol (newSymbol(symbol one)) newArg (funcType one)
    return (Signature (newFs:fs))

swap :: Type -> Type -> [Type] -> [Type]
swap _ _ [] = []
swap n m (x:xs)
  | n == x = m : swap n m xs
  | m == x = n : swap n m xs
  | otherwise = x : swap n m xs

twoDiffTypes :: [Type] -> Gen (Type,Type)
twoDiffTypes [] = error "This will never happen!"
twoDiffTypes ts = do
    a <- elements ts
    b <- elements ts
    if a == b
    then twoDiffTypes ts
    else return (a,b)

newSymbol :: String -> String
newSymbol s = s ++ "'"

