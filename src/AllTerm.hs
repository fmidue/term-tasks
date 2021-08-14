module AllTerm (
    allTerms,
    allTerms'
)where

import Test.QuickCheck
import DataType
import ArbitrarySig (arbSignature)
import ValidTerm (validTerms)
import InvalidTerm (invalidTerms,invalidTerms')
import Data.Maybe (catMaybes)

allTerms :: [String] -> [Type] -> [(Int,Error)] -> Int -> Int -> Int -> Int -> Int -> Gen (Signature,[[Term]])
allTerms s ts e n a b num1 num2 = do
    sig <- arbSignature s ts n
    let validT = validTerms sig Nothing a b
    invalidT <- invalidTerms sig e a b
    if length validT <= num1 || length invalidT <= num2
    then allTerms s ts e n a b num1 num2
    else return (sig,[validT, catMaybes invalidT])

allTerms' :: [String] -> [Type] -> Error -> Int -> Int -> Int -> Int -> Int -> Gen (Signature,[[Term]])
allTerms' s ts e n a b num1 num2 = do
    sig <- arbSignature s ts n
    let validT = validTerms sig Nothing a b
    invalidT <- invalidTerms' sig e a b
    if length validT <= num1 || length invalidT <= num2
    then allTerms' s ts e n a b num1 num2
    else return (sig,[validT,invalidT])
