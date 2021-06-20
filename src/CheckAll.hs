module CheckAll where
import DataType
import ValidCheck
import ComputeTerm
import ArbitraryTerm
import Test.QuickCheck

allTerm :: Int -> Signature -> Gen [Term]
allTerm n xs = do 
    a <- randomTerm n xs 
    let b = term n xs
    c <- shuffle (a++b) 
    let d = take n c    
    return d 

printAllTerm :: Int -> Signature -> Gen [String]
printAllTerm n xs = do 
    a <- printRanTerm n xs 
    let b = printTerm n xs 
    c <- shuffle (a++b)
    let d = take n c
    return d 

checkAllTerm :: Int -> Signature -> Gen [Bool] 
checkAllTerm n xs = do 
    a <- allTerm n xs 
    let b = map (isValid xs) a 
    return b 
