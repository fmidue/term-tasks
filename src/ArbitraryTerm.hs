module ArbitraryTerm where
import Test.QuickCheck
import DataType
import ArbitrarySig
import GetSignatureInfo (allFunctions)
import OneTerm (oneValidTerm)

oneInvalidTerm :: Signature -> Error -> Gen Term
oneInvalidTerm sig e = do
    newSig <- randomSig sig e
    let f = allFunctions newSig
    if null f
    then oneInvalidTerm sig e
    else oneValidTerm newSig

invalidTerm :: Signature -> [(Int,Error)] -> Gen [Term]
invalidTerm _ [] = return []
invalidTerm sig ((n,e):xs)
    | n == 0 = invalidTerm sig xs
    | otherwise = do
        nextTurn <- invalidTerm sig ((n-1,e):xs)
        thisTerm <- oneInvalidTerm sig e
        return (thisTerm : nextTurn)




