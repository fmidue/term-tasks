module ArbitrarySig (
    swapArgOrder,
    duplicateArg,
    oneMoreArg,
    oneLessArg,
    oneDiffType,
    arbSignature
)where
import DataType
import Test.QuickCheck
import Data.List (nub,delete)
import GetSignatureInfo (allTypes)

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

duplicateArg :: Signature -> Gen (Maybe(Signature,String))
duplicateArg (Signature fs) = do
    let available = filter (not. null. #arguments) fs
    if null available
    then return Nothing
    else do
        one <- elements available
        n <- chooseInt (0,length (#arguments one)-1)
        let newArg = duplicate n (#arguments one)
            newSym = newSymbol(#symbol one)
            newFs = Symbol newSym newArg (#result one)
        return (Just(Signature (newFs:fs),newSym))

duplicate :: Int -> [Type] -> [Type]
duplicate n ts = take n ts ++ [ts !! n] ++ drop n ts

oneMoreArg :: Signature -> Gen (Maybe(Signature,String))
oneMoreArg sig@(Signature fs) = do
    one <- elements fs
    oneType <- elements (allTypes sig)
    position <- chooseInt (0,length (#arguments one)-1)
    let newArg = newTypes position oneType (#arguments one)
        newSym = newSymbol(#symbol one)
        newFs = Symbol newSym newArg (#result one)
    return (Just(Signature (newFs:fs),newSym))

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
        t <- elements newList
        let newArg = replace position t (#arguments one)
            newSym = newSymbol(#symbol one)
            newFs = Symbol newSym newArg (#result one)
        return (Just(Signature (newFs:fs),newSym))


replace :: Int -> Type -> [Type] -> [Type]
replace n t' ts = [if i == n then t' else t | (i,t) <- zip [0..] ts ]

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





