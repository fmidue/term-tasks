module ArbitrarySig where
import DataType
import Test.QuickCheck
import Data.List (nub)

swapOrder :: Signature -> Gen Signature
swapOrder (Signature fs) = do
    let available = filter (\x -> length (nub (#arguments x)) >=2) fs
    one <- elements available
    (a,b) <- twoDiffPositions (length (#arguments one))
    let newArg = swap a b (#arguments one)
        newFs = FunctionSymbol (newSymbol(symbol one)) newArg (funcType one)
    return (Signature (newFs:fs))

swap :: Int -> Int -> [Type] -> [Type]
swap _ _ [] = []
swap n m xs = left ++ [b] ++ middle ++ [a] ++ right
                where a = xs !! n
                      b = xs !! m
                      left = take n xs
                      right = drop (m+1) xs
                      middle = take (m-n-1) (drop (n+1) xs)

twoDiffPositions :: Int -> Gen (Int,Int)
twoDiffPositions 0 = error "This will never happen!"
twoDiffPositions 1 = error "This will never happen!"
twoDiffPositions n = do
    a <- suchThat (chooseInt (0,n-1)) (<n-1)
    b <- suchThat (chooseInt (0,n-1)) (\x -> x/=a && x>a)
    return (a,b)

newSymbol :: String -> String
newSymbol s = s ++ "'"

duplicateArg :: Signature -> Gen Signature
duplicateArg (Signature fs) = do
    let available = filter (not. null. #arguments) fs
    one <- elements available
    n <- chooseInt (0,length (#arguments one)-1)
    let newArg = duplicate n (#arguments one)
        newFs = FunctionSymbol (newSymbol(symbol one)) newArg (funcType one)
    return (Signature (newFs:fs))

duplicate :: Int -> [Type] -> [Type]
duplicate _ [] = []
duplicate 0 (x:xs) = x : x : duplicate (-1) xs
duplicate n (x:xs) = x : duplicate (n-1) xs


