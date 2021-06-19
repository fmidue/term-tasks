module ArbitraryTerm where
import Test.QuickCheck
import DataType
import ComputeTerm 
import ArbitrarySig

totalRandomTerm :: Int -> Signature -> Gen [Term] 
totalRandomTerm n xs = do 
    a <- randomSigTotal xs 
    let b = term n a 
    return b 

printRanTerm :: Int -> Signature -> Gen [String] 
printRanTerm n xs = do 
    a <- randomSigTotal' xs 
    let b = printTerm n a 
    return b 

