module ArbitrarySig where
import DataType
import Test.QuickCheck
import Data.List 

-- Signature [FunctionSymbol]
-- data FunctionSymbol = FunctionSymbol String [Type] Type

randomSig :: Signature -> Gen Signature
randomSig (Signature []) = return (Signature [])
randomSig (Signature xs) = do 
    a <- randomSig' xs 
    return (Signature a)
    
randomSig' :: [FunctionSymbol] -> Gen [FunctionSymbol] 
randomSig' [] = return []
randomSig' (x:xs) = do 
    if checkSigTerm x 
    then do 
        a <- chooseInt (0,1)
        if a == 0 
        then do 
            b <- randomSig' xs
            return (x:b)
        else do 
            b <- changeSigName x 
            c <- randomSig' xs 
            return (b:c)
    else do     
        let a = ["changeOrder","changeLength","changeName"] 
        b <- elements a
        c <- makeFuncSymbol b x
        d <- randomSig' xs
        return (c : d)

makeFuncSymbol :: String -> FunctionSymbol -> Gen FunctionSymbol 
makeFuncSymbol "changeOrder" x = changeSigOrder x 
makeFuncSymbol "changeLength" x = changeSigLength x 
makeFuncSymbol "changeName" x = changeSigName x 
makeFuncSymbol _ _ = return (FunctionSymbol "Error" [] (Type "Error"))

randomSigTotal :: Signature -> Gen Signature 
randomSigTotal s = do 
    let a = getSignatureName s
        b = getSignatureName' s
        c = getSignatureType s
    d <- makeFuncTerm a c
    e <- makeFuncSymbol' b c  
    return (Signature (d++e))

makeFuncTerm :: [String] -> [Type] -> Gen [FunctionSymbol]
makeFuncTerm [] _ = return []
makeFuncTerm _ [] = return []
makeFuncTerm (x:xs) t = do 
    a <- elements t
    b <- makeFuncTerm xs t  
    return (FunctionSymbol x [] a : b) 

makeFuncSymbol' :: [String] -> [Type] -> Gen [FunctionSymbol]
makeFuncSymbol' [] _ = return []
makeFuncSymbol' _ [] = return []
makeFuncSymbol' (x:xs) t = do 
    a <- chooseInt (0,6)
    let b = elements t 
    c <- vectorOf a b 
    d <- elements t 
    e <- makeFuncSymbol' xs t 
    return (FunctionSymbol x c d : e)

randomSigTotal' :: Signature -> Gen Signature 
randomSigTotal' s = do 
    let a = getAllName s        
        b = getSignatureType s
    c <- makeFuncSymbol' a b   
    return (Signature c)

getAllName :: Signature -> [String] 
getAllName (Signature []) = []
getAllName (Signature (FunctionSymbol s _ _ : xs)) = s : getAllName (Signature xs)

getSignatureName :: Signature -> [String] 
getSignatureName (Signature []) = []
getSignatureName (Signature (FunctionSymbol s ys _ : xs)) 
    | null ys = s : getSignatureName (Signature xs) 
    | otherwise = getSignatureName (Signature xs) 

getSignatureName' :: Signature -> [String] 
getSignatureName' (Signature []) = []
getSignatureName' (Signature (FunctionSymbol s ys _ : xs)) 
    | null ys = getSignatureName' (Signature xs) 
    | otherwise = s : getSignatureName' (Signature xs) 

getSignatureType :: Signature -> [Type] 
getSignatureType (Signature []) = []
getSignatureType (Signature (FunctionSymbol _ x y : xs)) = nub (x++y:getSignatureType (Signature xs)) 

changeSigOrder :: FunctionSymbol -> Gen FunctionSymbol
changeSigOrder (FunctionSymbol s xs t)= do
    a <- shuffle xs 
    return (FunctionSymbol s a t)

changeSigLength :: FunctionSymbol -> Gen FunctionSymbol 
changeSigLength (FunctionSymbol s xs t)= do 
    a <- choose (0,(length xs)+5) 
    let b = elements xs 
    c <- vectorOf a b 
    return (FunctionSymbol s c t)

changeSigName :: FunctionSymbol -> Gen FunctionSymbol 
changeSigName (FunctionSymbol _ xs t) = do 
    let a = ['a'..'z']
        b = map makeString a
    c <- elements b 
    return (FunctionSymbol c xs t)

makeString :: Char -> String
makeString x = [x]

checkSigTerm :: FunctionSymbol -> Bool
checkSigTerm (FunctionSymbol _ xs _) 
   | null xs = True 
   | otherwise = False


