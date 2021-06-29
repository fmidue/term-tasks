module ArbitrarySig (
    randomSig,
    randomSig',
    makeFuncSymbol,
    randomSigTotal,
    makeFuncSymbol'
    ) where
import DataType
import GetSignatureInfo (getAllConstantSymbol,getFuncSymbol,getAllType,getAllConstant)
import Test.QuickCheck
import Data.List

-- only change the order or number of arguments, or change the name of functions
randomSig :: Signature -> Gen Signature
randomSig (Signature []) = return (Signature [])
randomSig sig = do
    let s = getAllConstantSymbol sig
    allFuncSym <- randomSig' sig s
    let finFuncSym = overlaps (Signature allFuncSym) sig
    return (Signature finFuncSym)

randomSig' :: Signature -> [String] -> Gen [FunctionSymbol] 
randomSig' (Signature []) _ = return []
randomSig' (Signature(x:xs)) s =  
    if checkConstant x
    then do
        nextFuncSym <- randomSig' (Signature xs) s
        return (x:nextFuncSym)
    else do
        let changeL = ["changeArgOrder","changeArgLength","changeFuncSymbol"]
        changeType <- elements changeL
        funcSym <- makeFuncSymbol changeType x s
        nextFuncSym <- randomSig' (Signature xs) s
        return (funcSym : nextFuncSym)

makeFuncSymbol :: String -> FunctionSymbol -> [String] -> Gen FunctionSymbol
makeFuncSymbol "changeArgOrder" x _ = changeArgOrder x
makeFuncSymbol "changeArgLength" x _ = changeArgLength x
makeFuncSymbol "changeFuncSymbol" x s = changeFuncSymbol x s
makeFuncSymbol _ _ _  = return (error "No FunctionSymbol can't be generated")

-- The ground terms won't be changed. The name of functions can't be changed.
-- But parameters of the functions will be completely randomly generated.
randomSigTotal :: Signature -> Gen Signature
randomSigTotal (Signature []) = return (Signature [])
randomSigTotal sig = do
    let s = getFuncSymbol sig
        t = getAllType sig
        constant = getAllConstant sig
    fsList <- makeFuncSymbol' s t
    let fsAll = constant ++ fsList
        f = overlaps (Signature fsAll) sig
    return (Signature f)

makeFuncSymbol' :: [String] -> [Type] -> Gen [FunctionSymbol]
makeFuncSymbol' [] _ = return []
makeFuncSymbol' _ [] = return []
makeFuncSymbol' (x:xs) t = do
    n <- chooseInt (0,5)
    let e = elements t
    fList <- vectorOf n e
    ft <- elements t
    nf <- makeFuncSymbol' xs t
    return (FunctionSymbol x fList ft : nf)

changeArgOrder :: FunctionSymbol -> Gen FunctionSymbol
changeArgOrder (FunctionSymbol s xs t)= do
    randomOrder <- shuffle xs
    return (FunctionSymbol s randomOrder t)

changeArgLength :: FunctionSymbol -> Gen FunctionSymbol
changeArgLength (FunctionSymbol s xs t)= do
    argLength <- choose (0,length xs + 2)
    let selectType = elements xs
    randomLength <- vectorOf argLength selectType
    return (FunctionSymbol s randomLength t)

changeFuncSymbol :: FunctionSymbol -> [String] -> Gen FunctionSymbol
changeFuncSymbol (FunctionSymbol _ xs t) s = do
    let symbols = ['a'..'z']
        symbols' = map makeString symbols \\ s
    newSymbol <- elements symbols'
    return (FunctionSymbol newSymbol xs t)

makeString :: Char -> String
makeString x = [x]

checkConstant :: FunctionSymbol -> Bool
checkConstant (FunctionSymbol _ xs _)
   | null xs = True
   | otherwise = False

checkSame :: FunctionSymbol -> Signature -> Bool
checkSame _ (Signature []) = False
checkSame (FunctionSymbol s1 x1 t1) (Signature(FunctionSymbol s2 x2 _:ys))
   | s1 == s2 && x1 == x2 = True
   | otherwise = checkSame (FunctionSymbol s1 x1 t1) (Signature ys)

overlaps :: Signature -> Signature -> [FunctionSymbol]
overlaps (Signature []) _ = []
overlaps _ (Signature []) = []
overlaps (Signature(FunctionSymbol s x t : xs)) ys
   | null x = FunctionSymbol s x t : overlaps (Signature xs) ys
   | checkSame (FunctionSymbol s x t) ys = overlaps (Signature xs) ys
   | otherwise = FunctionSymbol s x t : overlaps (Signature xs) ys
