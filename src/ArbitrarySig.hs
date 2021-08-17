module ArbitrarySig (
    swapArgOrder,
    oneMoreArg,
    oneLessArg,
    oneDiffType,
    wrongSymbol,
    wrongSymbol',
    arbSignature
)where
import DataType (Signature(..),Symbol(..),Type(..),allTypes,allArgsResults,allSymbols)
import Test.QuickCheck
import Data.List (nub,delete)

swapArgOrder :: Signature -> Gen (Maybe(Signature,String))
swapArgOrder sig@(Signature fs) = do
    let available = filter (\x -> length (nub (#arguments x)) >=2) fs
    if null available
    then return Nothing
    else do
        one <- elements available
        (a,b) <- twoDiffPositions (length (#arguments one))
        if #arguments one !! a == #arguments one !! b
        then swapArgOrder sig
        else do
          let newArg = swap a b (#arguments one)
              newSym = newSymbol(#symbol one)
              newFs = Symbol newSym newArg (#result one)
          return (Just(Signature (newFs:fs),newSym))

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
    a <- chooseInt (0,n-2)
    b <- chooseInt (0,n-1) `suchThat` (\x -> x/=a && x>a)
    return (a,b)

newSymbol :: String -> String
newSymbol s = s ++ "'"

oneMoreArg :: Signature -> Gen (Signature,String)
oneMoreArg sig@(Signature fs) = do
    one <- elements fs
    oneType <- elements (allTypes sig)
    position <- chooseInt (0,length (#arguments one)-1)
    let newArg = newTypes position oneType (#arguments one)
        newSym = newSymbol(#symbol one)
        newFs = Symbol newSym newArg (#result one)
    return (Signature (newFs:fs),newSym)

newTypes :: Int -> Type -> [Type] -> [Type]
newTypes n t' ts =  take n ts ++ [t'] ++ drop n ts

oneLessArg :: Signature -> Gen (Maybe(Signature,String))
oneLessArg (Signature fs) = do
    let available = filter (not. null. #arguments) fs
    if null available
    then return Nothing
    else do
        one <- elements available
        position <- chooseInt (0,length (#arguments one)-1)
        let newArg = newTypes' position (#arguments one)
            newSym = newSymbol(#symbol one)
            newFs = Symbol newSym newArg (#result one)
        return (Just(Signature (newFs:fs),newSym))

newTypes' :: Int -> [Type] -> [Type]
newTypes' n ts = [t | (i,t) <- zip [0..] ts, i /= n]

oneDiffType :: Signature -> Gen (Maybe(Signature,String))
oneDiffType sig@(Signature fs) = do
    let available = filter (not. null. #arguments) fs
    if null available
    then return Nothing
    else do
        one <- elements available
        position <- chooseInt (0,length (#arguments one)-1)
        let types = allTypes sig
            newList = delete (#arguments one !! position) types
        if null newList
        then return Nothing
        else do
            t <- elements newList
            let newArg = replace position t (#arguments one)
                newSym = newSymbol(#symbol one)
                newFs = Symbol newSym newArg (#result one)
            return (Just(Signature (newFs:fs),newSym))

replace :: Int -> Type -> [Type] -> [Type]
replace n t' ts = [if i == n then t' else t | (i,t) <- zip [0..] ts ]

wrongSymbol :: Signature -> Gen (Signature,String)
wrongSymbol (Signature fs) = do
    one <- elements fs
    let newSym = newSymbol(#symbol one)
        newSym' = newSymbol newSym
        newFs = Symbol newSym' (#arguments one) (#result one)
    return (Signature (newFs:fs),newSym')

wrongSymbol' :: Signature -> Gen (Signature,String)
wrongSymbol' sig@(Signature fs) = do
    let types = allTypes sig
        symbols = allSymbols sig
        argsAndRes = allArgsResults sig
        lengths = map (length . fst) argsAndRes
    l <- chooseInt (0,maximum lengths)
    typeList <- vectorOf l (elements types)
    t <- elements types
    newSym <- elements symbols
    if (typeList,t) `elem` argsAndRes
    then wrongSymbol' sig
    else do
        let newSym' = newSymbol newSym
            newSym'' = newSymbol newSym'
            newFs = Symbol newSym'' typeList t
        return (Signature (newFs:fs),newSym'')

arbSignature :: [String] -> [Type] -> Int -> Gen Signature
arbSignature s ts n = do
    symbols <- arbSymbols s ts n
    return (Signature symbols)

arbSymbols :: [String] -> [Type] -> Int -> Gen [Symbol]
arbSymbols [] _ _ = return []
arbSymbols (s:ls) ts n = do
    l <- elements [0..n]
    typeList <- vectorOf l (elements ts)
    t <- elements ts
    nextSymbol <- arbSymbols ls ts n
    let thisSymbol = Symbol s typeList t
    return (thisSymbol:nextSymbol)





