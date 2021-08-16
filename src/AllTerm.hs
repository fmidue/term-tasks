module AllTerm (
    allTerms
)where

import Test.QuickCheck
import DataType (Signature(..),Type(..),Term(..),Error(..))
import ArbitrarySig (arbSignature)
import ValidTerm (validTerms)
import InvalidTerm (invalidTerms)

allTerms :: [String] -> [Type] -> [(Int,Error)] -> Int -> Int -> Int -> Int -> Gen (Signature,([Term],[[Term]]))
allTerms s ts e n a b num = do
    sig <- arbSignature s ts n
    let validT = validTerms sig Nothing a b
    invalidT <- invalidTerms sig e a b
    if length validT >= num && theLength invalidT == theSum e
    then return (sig,(validT, invalidT))
    else allTerms s ts e n a b num

theSum :: [(Int,Error)] -> Int
theSum xs = sum (map fst xs)

theLength :: [[Term]] -> Int
theLength xs = sum (map length xs)


