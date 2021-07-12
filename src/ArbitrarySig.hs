module ArbitrarySig (
    randomSig,
    longerLength
    ) where
import DataType
import GetSignatureInfo (allSymbols,allTypes)
import Test.QuickCheck
import Data.List

-- only change the order or number of arguments, or change the name of functions
randomSig :: Signature -> Error -> Gen Signature
randomSig (Signature []) _ = return (Signature [])
randomSig sig e = do
    let s = allSymbols sig
        t = allTypes sig
    allFuncSym <- randomSig' sig e s t
    let finFuncSym = overlaps (Signature allFuncSym) sig
    return (Signature finFuncSym)

randomSig' :: Signature -> Error -> [String] -> [Type] -> Gen [FunctionSymbol] 
randomSig' (Signature []) _ _ _ = return []
randomSig' (Signature(x:xs)) e s t = do
   if isConstant x
   then do nextFuncSym <- randomSig' (Signature xs) e s t
           return (x:nextFuncSym)
   else do nextFuncSym <- randomSig' (Signature xs) e s t
           funcSym <- newFunctionSymbols x e s t
           return (funcSym:nextFuncSym)

newFunctionSymbols :: FunctionSymbol -> Error -> [String] -> [Type] -> Gen FunctionSymbol
newFunctionSymbols x e s t
   | e == ORDER = newArgOrder x
   | e == LENGTH = newArgLength x
   | e == TYPE = newArgType x t
   | e == SYMBOL = newFuncName x s
   | otherwise = error "No FunctionSymbol will be generated"

newArgOrder :: FunctionSymbol -> Gen FunctionSymbol
newArgOrder (FunctionSymbol s xs t) = do
    randomOrder <- shuffle xs
    return (FunctionSymbol s randomOrder t)

newArgLength :: FunctionSymbol -> Gen FunctionSymbol
newArgLength (FunctionSymbol s xs t) = do
    argLength <- choose (0,length xs + 2)
    let n = abs (length xs - argLength)
    if argLength >= length xs
    then do randomLength <- longerLength xs n
            return (FunctionSymbol s randomLength t)
    else do randomLength <- shorterLength xs n
            return (FunctionSymbol s randomLength t)

shorterLength :: [Type] -> Int -> Gen [Type]
shorterLength ts 0 = return ts
shorterLength ts n = do
    t <- elements ts
    shorterLength (delete t ts) (n-1)


longerLength :: [Type] -> Int -> Gen [Type]
longerLength ts 0 = return ts
longerLength ts n = do
    t <- elements ts
    longerLength (ts ++ [t]) (n-1)

newArgType :: FunctionSymbol -> [Type] -> Gen FunctionSymbol
newArgType (FunctionSymbol s xs t) ts = do
    n <- chooseInt (1,length xs)
    newList <- newArgType' xs n ts
    return (FunctionSymbol s newList t)

newArgType' :: [Type] -> Int -> [Type] -> Gen [Type]
newArgType' t 0 _ = return t
newArgType' t n ts = do
    n' <- chooseInt (1,length t)
    newType <- elements ts
    let newList = replace n' newType t
    newArgType' newList (n-1) ts

replace :: Int -> Type -> [Type] -> [Type]
replace _ _ [] = []
replace n y (x:xs)
  | n == 0 = y : replace (n-1) y xs
  | otherwise = x : replace (n-1) y xs

newFuncName :: FunctionSymbol -> [String] -> Gen FunctionSymbol
newFuncName (FunctionSymbol _ xs t) s = do
    let symbols = ['a'..'z']
        symbols' = map toString symbols \\ s
    newSymbol <- elements symbols'
    return (FunctionSymbol newSymbol xs t)

toString :: Char -> String
toString x = [x]

isConstant :: FunctionSymbol -> Bool
isConstant (FunctionSymbol _ xs _) = null xs

isSame :: FunctionSymbol -> Signature -> Bool
isSame _ (Signature []) = False
isSame (FunctionSymbol s1 x1 t1) (Signature(FunctionSymbol s2 x2 _:ys))
   | s1 == s2 && x1 == x2 = True
   | otherwise = isSame (FunctionSymbol s1 x1 t1) (Signature ys)

overlaps :: Signature -> Signature -> [FunctionSymbol]
overlaps (Signature []) _ = []
overlaps _ (Signature []) = []
overlaps (Signature(FunctionSymbol s x t : xs)) ys
   | null x = FunctionSymbol s x t : overlaps (Signature xs) ys
   | isSame (FunctionSymbol s x t) ys = overlaps (Signature xs) ys
   | otherwise = FunctionSymbol s x t : overlaps (Signature xs) ys
