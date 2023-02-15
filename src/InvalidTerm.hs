{-# LANGUAGE NamedFieldPuns #-}

module InvalidTerm (
  invalidTerms,
)where

import Test.QuickCheck (Gen, elements, suchThat, chooseInt)
import DataType (Signature(..), Symbol(..), Term(..), Error(..), Type, allTypes, allArgsResults, allSymbols)
import Auxiliary (different)
import ValidTerm (validTerms)
import Data.List (nub,delete,(\\))
import Control.Monad (replicateM)

swapArgOrder :: Signature -> Gen (Maybe (Signature, String))
swapArgOrder sig@(Signature fs) = do
    let available = filter (\x -> length (nub (#arguments x)) >= 2) fs
    if null available
    then return Nothing
    else do
        Symbol{symbol, arguments, result} <- elements available
        (a,b) <- twoDiffPositions (length arguments)
        if arguments !! a == arguments !! b
        then swapArgOrder sig
        else do
          let newArg = swap a b arguments
              newSym = newSymbol symbol
              newFs = Symbol newSym newArg result
          return (Just (Signature (newFs : fs), newSym))

swap :: Int -> Int -> [Type] -> [Type]
swap n m xs = left ++ [b] ++ middle ++ [a] ++ right
                where a = xs !! n
                      b = xs !! m
                      left = take n xs
                      right = drop (m+1) xs
                      middle = take (m-n-1) (drop (n+1) xs)

twoDiffPositions :: Int -> Gen (Int, Int)
twoDiffPositions n | n < 2 = error "This will never happen!"
twoDiffPositions n = do
    a <- chooseInt (0,n-1)
    b <- chooseInt (0,n-1) `suchThat` (/= a)
    return (min a b, max a b)

newSymbol :: String -> String
newSymbol s = s ++ "'"

oneMoreArg :: Signature -> Gen (Signature, String)
oneMoreArg sig@(Signature fs) = do
    Symbol{symbol, arguments, result} <- elements fs
    oneType <- elements (allTypes sig)
    position <- chooseInt (0, length arguments - 1)
    let newArg = take position arguments ++ [oneType] ++ drop position arguments
        newSym = newSymbol symbol
        newFs = Symbol newSym newArg result
    return (Signature (newFs : fs), newSym)

oneLessArg :: Signature -> Gen (Maybe (Signature, String))
oneLessArg (Signature fs) = do
    let available = filter (not . null . #arguments) fs
    if null available
    then return Nothing
    else do
        Symbol{symbol, arguments, result} <- elements available
        position <- chooseInt (0, length arguments - 1)
        let newArg = [ t | (i, t) <- zip [0..] arguments, i /= position ]
            newSym = newSymbol symbol
            newFs = Symbol newSym newArg result
        return (Just (Signature (newFs : fs), newSym))

oneDiffType :: Signature -> Gen (Maybe (Signature, String))
oneDiffType sig@(Signature fs) = do
    let available = filter (not . null . #arguments) fs
    if null available
    then return Nothing
    else do
        Symbol{symbol, arguments, result} <- elements available
        position <- chooseInt (0, length arguments - 1)
        let types = allTypes sig
            newList = delete (arguments !! position) types
        if null newList
        then return Nothing
        else do
            t' <- elements newList
            let newArg = [ if i == position then t' else t | (i, t) <- zip [0..] arguments ]
                newSym = newSymbol symbol
                newFs = Symbol newSym newArg result
            return (Just (Signature (newFs : fs), newSym))

wrongSymbol :: Signature -> Gen (Signature, String)
wrongSymbol (Signature fs) = do
    Symbol{symbol, arguments, result} <- elements fs
    let newSym = newSymbol symbol
        newSym' = newSymbol newSym
        newFs = Symbol newSym' arguments result
    return (Signature (newFs : fs), newSym')

wrongSymbol' :: Signature -> Gen (Maybe (Signature, String))
wrongSymbol' sig@(Signature fs) = do
    let types = allTypes sig
        argsAndRes = allArgsResults sig
        lengths = map (length . fst) argsAndRes
        available = combination (maximum lengths) types \\ argsAndRes
    if null available
    then return Nothing
    else do
        let symbols = allSymbols sig
        (typeList, t) <- elements available
        newSym <- elements symbols
        let newSym' = newSymbol newSym
            newSym'' = newSymbol newSym'
            newFs = Symbol newSym'' typeList t
        return (Just (Signature (newFs : fs), newSym''))

combination :: Int -> [Type] -> [([Type],Type)]
combination n ts = [(x,t) | i <- reverse [0..n], t <- ts, x <- replicateM i ts ]

newSignature :: Signature -> Error -> Gen (Maybe (Signature, String))
newSignature sig Swap = swapArgOrder sig
newSignature sig OneMore = fmap Just (oneMoreArg sig)
newSignature sig OneLess = oneLessArg sig
newSignature sig TypeChange = oneDiffType sig
newSignature sig NameTypo = fmap Just (wrongSymbol sig)
newSignature sig UnknownSymbol = wrongSymbol' sig

originalSymbol :: String -> Term -> Term
originalSymbol s' (Term s ts)
  | s == s' = Term (init s) (map (originalSymbol s') ts)
  | otherwise = Term s (map (originalSymbol s') ts)

invalidTerms :: Signature -> Int -> Int -> Maybe [Type] -> [(Int,Error)] -> Gen [[Term]]
invalidTerms sig a b root =
  traverse (\(n,e) ->
               invalidTerms' sig a b root e >>=
               \terms -> different terms (min n (length terms))
           )

invalidTerms' :: Signature -> Int -> Int -> Maybe [Type] -> Error -> Gen [Term]
invalidTerms' sig a b root e = do
    new <- newSignature sig e
    case new of
      Nothing -> return []
      Just (newSig,s) -> do
        let terms = validTerms newSig (Just s) a b root
            terms' = map (originalSymbol s) terms
        return terms'
