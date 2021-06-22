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
    checkSigTerm,
    overlaps
    ) where
import DataType
import ValidCheck
import Test.QuickCheck
import Data.List 

-- only change the order or number of arguments, or change the name of functions 
randomSig :: Signature -> Gen Signature
randomSig (Signature []) = return (Signature [])
randomSig (Signature xs) = do 
    let e = getSigTermName xs 
    a <- randomSig' xs e 
    let b = overlaps a xs 
--    let b = getSignatureTerm (Signature xs)       
--        c = overlaps a xs          
--        d = (a \\ c)
    return (Signature b)
       
randomSig' :: [FunctionSymbol] -> [String] -> Gen [FunctionSymbol] 
randomSig' [] _ = return []
randomSig' (x:xs) s =  
    if checkSigTerm x 
    then do 
        b <- randomSig' xs s 
        return (x:b)
    else do     
        let a = ["changeOrder","changeLength","changeName"] 
        b <- elements a
        c <- makeFuncSymbol b x s 
        d <- randomSig' xs s
        return (c : d)

makeFuncSymbol :: String -> FunctionSymbol -> [String] -> Gen FunctionSymbol 
makeFuncSymbol "changeOrder" x _ = changeSigOrder x 
makeFuncSymbol "changeLength" x _ = changeSigLength x 
makeFuncSymbol "changeName" x s = changeSigName x s
makeFuncSymbol _ _ _  = return (FunctionSymbol "Error" [] (Type "Error"))

-- The ground terms won't be changed. The name of functions can't be changed. 
-- But parameters of the functions will be completely randomly generated. 
randomSigTotal :: Signature -> Gen Signature 
randomSigTotal (Signature []) = return (Signature [])
randomSigTotal (Signature xs) = do 
    let a = getSignatureName (Signature xs) 
        b = getSignatureType (Signature xs)
        c = getSignatureTerm (Signature xs)
    d <- makeFuncSymbol' a b 
    let e = c ++ d 
        f = overlaps e xs 
--    let e = overlaps d (xs\\c)          
--        f = (d \\ e)
    return (Signature f)

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
randomSigTotal' (Signature xs) = do 
    let a = getAllName (Signature xs)        
        b = getSignatureType (Signature xs)
    c <- makeFuncSymbol' a b  
    let d = overlaps c xs 
--        e = c \\ d 
    return (Signature d)

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

changeSigName :: FunctionSymbol -> [String] -> Gen FunctionSymbol 
changeSigName (FunctionSymbol _ xs t) s = do 
    let a = ['a'..'z']
        b = (map makeString a)\\s 
    c <- elements b 
    return (FunctionSymbol c xs t)

makeString :: Char -> String
makeString x = [x]

checkSigTerm :: FunctionSymbol -> Bool
checkSigTerm (FunctionSymbol _ xs _) 
   | null xs = True 
   | otherwise = False

checkExist :: FunctionSymbol -> [FunctionSymbol] -> Bool 
checkExist _ [] = False 
checkExist (FunctionSymbol s1 x1 t1) (FunctionSymbol s2 x2 _:ys) 
   | s1 == s2 && x1 == x2 = True 
   | otherwise = False || checkExist (FunctionSymbol s1 x1 t1) ys  

overlaps :: [FunctionSymbol] -> [FunctionSymbol] -> [FunctionSymbol]
overlaps [] _ = []
overlaps _ [] = []
overlaps (FunctionSymbol s x t : xs) ys 
   | null x = (FunctionSymbol s x t) : overlaps xs ys 
   | checkExist (FunctionSymbol s x t) ys = overlaps xs ys 
   | otherwise = (FunctionSymbol s x t) : overlaps xs ys 
