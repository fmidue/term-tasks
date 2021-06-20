module ArbitrarySig (
    randomSig,
    randomSig',
    makeFuncSymbol,
    randomSigTotal,
    makeFuncSymbol',
    randomSigTotal',
    getAllName,
    getSignatureTerm,
    getSignatureName,
    getSignatureType,
    changeSigOrder,
    changeSigLength,
    changeSigName,
    makeString,
    checkSigTerm
    ) where
import DataType
import Test.QuickCheck
import Data.List 

-- only change the order or number of arguments, or change the name of functions 
randomSig :: Signature -> Gen Signature
randomSig (Signature []) = return (Signature [])
randomSig (Signature xs) = do 
    a <- randomSig' xs 
    return (Signature a)
    
randomSig' :: [FunctionSymbol] -> Gen [FunctionSymbol] 
randomSig' [] = return []
randomSig' (x:xs) =  
    if checkSigTerm x 
    then do 
        b <- randomSig' xs
        return (x:b)
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

-- The ground terms won't be changed. The name of functions can't be changed. 
-- But parameters of the functions will be completely randomly generated. 
randomSigTotal :: Signature -> Gen Signature 
randomSigTotal (Signature []) = return (Signature [])
randomSigTotal s = do 
    let a = getSignatureName s 
        b = getSignatureType s
        c = getSignatureTerm s
    d <- makeFuncSymbol' a b 
    return (Signature (c++d))

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

-- It only uses the names and types of original signature. 
-- It will generate totally new functions and ground terms.
randomSigTotal' :: Signature -> Gen Signature 
randomSigTotal' s = do 
    let a = getAllName s        
        b = getSignatureType s
    c <- makeFuncSymbol' a b   
    return (Signature c)

getAllName :: Signature -> [String] 
getAllName (Signature []) = []
getAllName (Signature (FunctionSymbol s _ _ : xs)) = s : getAllName (Signature xs)

getSignatureTerm :: Signature -> [FunctionSymbol] 
getSignatureTerm (Signature []) = []
getSignatureTerm (Signature (FunctionSymbol s ys t : xs)) 
    | null ys = (FunctionSymbol s ys t) : getSignatureTerm (Signature xs) 
    | otherwise = getSignatureTerm (Signature xs) 

getSignatureName :: Signature -> [String] 
getSignatureName (Signature []) = []
getSignatureName (Signature (FunctionSymbol s ys _ : xs)) 
    | null ys = getSignatureName (Signature xs) 
    | otherwise = s : getSignatureName (Signature xs) 

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

