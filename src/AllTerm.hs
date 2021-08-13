module AllTerm (
    allTerms
)where

import Test.QuickCheck
import DataType
import ArbitrarySig (arbSignature)
import ValidTerm (validTerms)
import InvalidTerm (invalidTerms')

allTerms :: [String] -> [Type] -> Error -> Int -> Int -> Int -> Gen [[Term]]
allTerms s ts e n a b = do
    sig <- arbSignature s ts n
    let validT = validTerms sig Nothing a b
    invalidT <- invalidTerms' sig e a b
    if null validT || null invalidT
    then allTerms s ts e n a b
    else return [validT,invalidT]
