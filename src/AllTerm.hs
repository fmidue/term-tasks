module AllTerm (
    allTerms
)where

import Test.QuickCheck (Gen)
import DataType (Signature, Type, Term, Error)
import ArbitrarySig (arbSignature)
import ValidTerm (validTerms)
import InvalidTerm (invalidTerms)

allTerms :: [String] -> [Type] -> [(Int,Error)] -> Int -> Int -> Int -> Int -> Gen (Signature,([Term],[[Term]]))
allTerms s ts e n a b num = do
    sig <- arbSignature s ts n
    let validT = validTerms sig Nothing a b
    invalidT <- invalidTerms sig e a b
    if length validT >= num && sum (map length invalidT)  == sum (map fst e)
    then return (sig,(validT, invalidT))
    else allTerms s ts e n a b num




