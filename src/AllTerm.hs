module AllTerm (
    allTerms
)where

import Test.QuickCheck (Gen)
import DataType (Signature, Type, Term, Error)
import ArbitrarySig (arbSignature)
import ValidTerm (validTerms)
import InvalidTerm (invalidTerms)

allTerms :: [String] -> [Type] -> [(Int,Error)] -> Int -> Int -> Int -> Int -> Gen (Signature,([Term String],[[Term String]]))
allTerms s ts e n a b num = do
    sig <- arbSignature s ts n
    let validT = validTerms sig Nothing a b Nothing
    invalidT <- invalidTerms sig a b Nothing e
    if length validT >= num && map length invalidT == map fst e
    then return (sig,(validT, invalidT))
    else allTerms s ts e n a b num
