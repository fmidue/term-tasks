module ArbitraryTerm (
   invalidTerm,
   invalidTerm'
   ) where
import Test.QuickCheck
import DataType
import ComputeTerm (makeSubterm,makeConstants)
import ArbitrarySig
import GetSignatureInfo (getAllConstantSymbol)

invalidTerm :: Int -> Error -> Signature -> Gen [Term]
invalidTerm n e sig = do
    sig' <- randomSig sig e
    let conSymbol = getAllConstantSymbol sig
        constant = makeConstants conSymbol
        getAllT = constant ++ makeSubterm n sig' constant
    return getAllT

invalidTerm' :: Signature -> [(Int,Error)] -> Gen [Term]
invalidTerm' _ [] = return []
invalidTerm' sig ((n,e):xs) = do
    a <- invalidTerm n e sig
    let b = elements a
    c <- vectorOf n b
    d <- invalidTerm' sig xs
    return (c ++ d)

