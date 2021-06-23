module ArbitraryTerm (
   randomTerm,
   printRanTerm,
   totalRandomTerm,
   printTotalRanTerm,
   totalRandomTerm',
   printTotalRanTerm'
   ) where
import Test.QuickCheck
import DataType
import ComputeTerm
import ArbitrarySig

randomTerm :: Int -> Signature -> Gen [Term]
randomTerm n xs = do
    a <- randomSig xs
    let b = term n a
    return b

printRanTerm :: Int -> Signature -> Gen [String]
printRanTerm n xs = do
    a <- randomSig xs
    let b = printTerm n a
    return b

totalRandomTerm :: Int -> Signature -> Gen [Term]
totalRandomTerm n xs = do
    a <- randomSigTotal xs
    let b = term n a
    return b

printTotalRanTerm :: Int -> Signature -> Gen [String]
printTotalRanTerm n xs = do
    a <- randomSigTotal xs
    let b = printTerm n a
    return b

totalRandomTerm' :: Int -> Signature -> Gen [Term]
totalRandomTerm' n xs = do
    a <- randomSigTotal xs
    let b = term n a
    return b

printTotalRanTerm' :: Int -> Signature -> Gen [String]
printTotalRanTerm' n xs = do
    a <- randomSigTotal' xs
    let b = printTerm n a
    return b
