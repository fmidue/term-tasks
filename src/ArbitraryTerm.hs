module ArbitraryTerm (
   invalidTerm,
   invalidTerm'
   ) where
import Test.QuickCheck
import Data.List (nub)
import DataType
import ComputeTerm (makeSubterm,makeConstants)
import ArbitrarySig
import GetSignatureInfo (getAllConstantSymbol)

invalidTerm :: Int -> Error -> Signature -> Gen [Term]
invalidTerm n e sig = do
    sig' <- randomSig sig e
    let conSymbol = getAllConstantSymbol sig
        constant = makeConstants conSymbol
        getAllT = makeSubterm n sig' constant
    return getAllT

invalidTerm' :: Signature -> [(Int,Error)] -> Gen [Term]
invalidTerm' _ [] = return []
invalidTerm' sig ((n,e):xs) = do
    allTerm <- invalidTerm n e sig
    if null allTerm
    then invalidTerm' sig xs
    else do
        let oneTerm = elements allTerm
        termList <- vectorOf n oneTerm
        let termList' = nub termList
        nextTurn <- invalidTerm' sig xs
        return (termList' ++ nextTurn)

