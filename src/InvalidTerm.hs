{-# LANGUAGE TupleSections #-}
module InvalidTerm (
  invalidTerms,
)where

import Test.QuickCheck (Gen, elements, suchThat, chooseInt)
import DataType (Signature(..), Symbol(..), Term(..), Error(..), Type, allTypes, allArgsResults, allSymbols)
import Auxiliary (different)
import ValidTerm (validTerms)
import Data.List (nub,delete,(\\))
import Control.Monad (replicateM)

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

wrongSymbol' :: Signature -> Gen (Maybe(Signature,String))
wrongSymbol' sig@(Signature fs) = do
    let types = allTypes sig
        argsAndRes = allArgsResults sig
        lengths = map (length . fst) argsAndRes
        available = (combination (maximum lengths) types) \\ argsAndRes
    if null available
    then return Nothing
    else do
        let symbols = allSymbols sig
        (typeList,t) <- elements available
        newSym <- elements symbols
        let newSym' = newSymbol newSym
            newSym'' = newSymbol newSym'
            newFs = Symbol newSym'' typeList t
        return (Just(Signature (newFs:fs),newSym''))

combination :: Int -> [Type] -> [([Type],Type)]
combination 0 ts = map ([],) ts
combination n ts = concatMap (\t-> [(,t) x | x <- replicateM n ts]) ts ++ combination (n-1) ts

newSignature :: Signature -> Error -> Gen (Maybe(Signature,String))
newSignature sig SWAP = swapArgOrder sig
newSignature sig ONEMORE = fmap Just (oneMoreArg sig)
newSignature sig ONELESS = oneLessArg sig
newSignature sig TYPE = oneDiffType sig
newSignature sig SYMBOL = fmap Just (wrongSymbol sig)
newSignature sig SYMBOLTYPE = wrongSymbol' sig

originalSymbol :: String -> Term -> Term
originalSymbol s' (Term s ts)
  | s == s' = Term (init s) ts
  | otherwise = Term s (map (originalSymbol s') ts)

invalidTerms :: Signature -> [(Int,Error)] -> Int -> Int -> Gen [[Term]]
invalidTerms _ [] _ _ = return []
invalidTerms sig ((n,e):ls) a b = do
  terms <- invalidTerms' sig e a b
  terms' <- different terms (min n (length terms))
  nextTerm <- invalidTerms sig ls a b
  return (terms':nextTerm)

invalidTerms' :: Signature -> Error -> Int-> Int -> Gen [Term]
invalidTerms' sig e a b = do
    new <- newSignature sig e
    case new of
      Nothing -> return []
      Just (newSig,s) -> do
        let terms = validTerms newSig (Just s) a b
            terms' = map (originalSymbol s) terms
        return terms'