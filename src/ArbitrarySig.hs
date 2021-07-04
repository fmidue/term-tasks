module ArbitrarySig (
    randomSig
    ) where
import DataType
import GetSignatureInfo (getAllConstantSymbol,getAllType)
import Test.QuickCheck
import Data.List

-- only change the order or number of arguments, or change the name of functions
randomSig :: Signature -> Error -> Gen Signature
randomSig (Signature []) _ = return (Signature [])
randomSig sig e = do
    let s = getAllConstantSymbol sig
        t = getAllType sig
    allFuncSym <- randomSig' sig e s t
    let finFuncSym = overlaps (Signature allFuncSym) sig
    return (Signature finFuncSym)

randomSig' :: Signature -> Error -> [String] -> [Type] -> Gen [FunctionSymbol] 
randomSig' (Signature []) _ _ _ = return []
randomSig' (Signature(x:xs)) e s t = do
   if checkConstant x
   then do nextFuncSym <- randomSig' (Signature xs) e s t
           return (x:nextFuncSym)
   else do nextFuncSym <- randomSig' (Signature xs) e s t
           funcSym <- makeFunctionSymbol x e s t
           return (funcSym:nextFuncSym)

makeFunctionSymbol :: FunctionSymbol -> Error -> [String] -> [Type] -> Gen FunctionSymbol
makeFunctionSymbol x e s t
   | e == ORDER = changeArgOrder x
   | e == LENGTH = changeArgLength x
   | e == TYPE = changeArgType x t
   | e == SYMBOL = changeFuncSymbol x s
   | otherwise = error "No FunctionSymbol will be generated"

changeArgOrder :: FunctionSymbol -> Gen FunctionSymbol
changeArgOrder (FunctionSymbol s xs t) = do
    randomOrder <- shuffle xs
    return (FunctionSymbol s randomOrder t)

changeArgLength :: FunctionSymbol -> Gen FunctionSymbol
changeArgLength (FunctionSymbol s xs t) = do
    argLength <- choose (0,length xs + 2)
    let selectType = elements xs
    randomLength <- vectorOf argLength selectType
    return (FunctionSymbol s randomLength t)

changeArgType :: FunctionSymbol -> [Type] -> Gen FunctionSymbol
changeArgType (FunctionSymbol s xs t) ts = do
    n <- chooseInt (1,length xs)
    newList <- changeArgType' xs n ts
    return (FunctionSymbol s newList t)

changeArgType' :: [Type] -> Int -> [Type] -> Gen [Type]
changeArgType' t 0 _ = return t
changeArgType' t n ts = do
    n' <- chooseInt (1,length t)
    newType <- elements ts
    let newList = replace n' newType t
    changeArgType' newList (n-1) ts

replace :: Int -> Type -> [Type] -> [Type]
replace _ _ [] = []
replace n y (x:xs)
  | n == 0 = y : replace (n-1) y xs
  | otherwise = x : replace (n-1) y xs

changeFuncSymbol :: FunctionSymbol -> [String] -> Gen FunctionSymbol
changeFuncSymbol (FunctionSymbol _ xs t) s = do
    let symbols = ['a'..'z']
        symbols' = map makeString symbols \\ s
    newSymbol <- elements symbols'
    return (FunctionSymbol newSymbol xs t)

makeString :: Char -> String
makeString x = [x]

checkConstant :: FunctionSymbol -> Bool
checkConstant (FunctionSymbol _ xs _) = null xs

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
