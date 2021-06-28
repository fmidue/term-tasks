module ArbitrarySig (
    randomSig,
    randomSig',
    makeFuncSymbol,
    randomSigTotal,
    makeFuncSymbol',
    randomSigTotal'
    ) where
import DataType
import GetSignatureInfo (getAllConstantSymbol,getSigSymbol,getFuncSymbol,getAllType)
import Test.QuickCheck
import Data.List

-- only change the order or number of arguments, or change the name of functions
randomSig :: Signature -> Gen Signature
randomSig (Signature []) = return (Signature [])
randomSig (Signature xs) = do
    let e = getAllConstantSymbol xs
    a <- randomSig' xs e
    let b = overlaps a xs
    return (Signature b)

randomSig' :: [FunctionSymbol] -> [String] -> Gen [FunctionSymbol] 
randomSig' [] _ = return []
randomSig' (x:xs) s =  
    if checkConstant x
    then do
        b <- randomSig' xs s
        return (x:b)
    else do
        let a = ["changeArgOrder","changeArgLength","changeFuncSymbol"]
        b <- elements a
        c <- makeFuncSymbol b x s
        d <- randomSig' xs s
        return (c : d)

makeFuncSymbol :: String -> FunctionSymbol -> [String] -> Gen FunctionSymbol
makeFuncSymbol "changeArgOrder" x _ = changeArgOrder x
makeFuncSymbol "changeArgLength" x _ = changeArgLength x
makeFuncSymbol "changeFuncSymbol" x s = changeFuncSymbol x s
makeFuncSymbol _ _ _  = return (FunctionSymbol "Error" [] (Type "Error"))

-- The ground terms won't be changed. The name of functions can't be changed.
-- But parameters of the functions will be completely randomly generated.
randomSigTotal :: Signature -> Gen Signature
randomSigTotal (Signature []) = return (Signature [])
randomSigTotal (Signature xs) = do
    let a = getFuncSymbol (Signature xs)
        b = getAllType (Signature xs)
        c = getSignatureTerm (Signature xs)
    d <- makeFuncSymbol' a b
    let e = c ++ d
        f = overlaps e xs
    return (Signature f)

makeFuncSymbol' :: [String] -> [Type] -> Gen [FunctionSymbol]
makeFuncSymbol' [] _ = return []
makeFuncSymbol' _ [] = return []
makeFuncSymbol' (x:xs) t = do
    a <- chooseInt (0,5)
    let b = elements t
    c <- vectorOf a b
    d <- elements t
    e <- makeFuncSymbol' xs t
    return (FunctionSymbol x c d : e)

-- It only uses the names and types of original signature.
-- It will generate totally new functions and ground terms.
randomSigTotal' :: Signature -> Gen Signature
randomSigTotal' (Signature xs) = do
    let a = getSigSymbol xs
        b = getAllType (Signature xs)
    c <- makeFuncSymbol' a b
    let d = overlaps c xs
--        e = c \\ d
    return (Signature d)

getSignatureTerm :: Signature -> [FunctionSymbol]
getSignatureTerm (Signature []) = []
getSignatureTerm (Signature (FunctionSymbol s ys t : xs))
    | null ys = (FunctionSymbol s ys t) : getSignatureTerm (Signature xs)
    | otherwise = getSignatureTerm (Signature xs)

changeArgOrder :: FunctionSymbol -> Gen FunctionSymbol
changeArgOrder (FunctionSymbol s xs t)= do
    a <- shuffle xs
    return (FunctionSymbol s a t)

changeArgLength :: FunctionSymbol -> Gen FunctionSymbol
changeArgLength (FunctionSymbol s xs t)= do
    a <- choose (0,(length xs)+2)
    let b = elements xs
    c <- vectorOf a b
    return (FunctionSymbol s c t)

changeFuncSymbol :: FunctionSymbol -> [String] -> Gen FunctionSymbol
changeFuncSymbol (FunctionSymbol _ xs t) s = do
    let a = ['a'..'z']
        b = (map makeString a)\\s
    c <- elements b
    return (FunctionSymbol c xs t)

makeString :: Char -> String
makeString x = [x]

checkConstant :: FunctionSymbol -> Bool
checkConstant (FunctionSymbol _ xs _)
   | null xs = True
   | otherwise = False

checkSame :: FunctionSymbol -> [FunctionSymbol] -> Bool
checkSame _ [] = False
checkSame (FunctionSymbol s1 x1 t1) (FunctionSymbol s2 x2 _:ys)
   | s1 == s2 && x1 == x2 = True
   | otherwise = False || checkSame (FunctionSymbol s1 x1 t1) ys

overlaps :: [FunctionSymbol] -> [FunctionSymbol] -> [FunctionSymbol]
overlaps [] _ = []
overlaps _ [] = []
overlaps (FunctionSymbol s x t : xs) ys
   | null x = (FunctionSymbol s x t) : overlaps xs ys
   | checkSame (FunctionSymbol s x t) ys = overlaps xs ys
   | otherwise = (FunctionSymbol s x t) : overlaps xs ys
