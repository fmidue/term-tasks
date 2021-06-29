module ArbitraryTerm (
   invalidTerm1,
   invalidTerm2,
   invalidTermOfType1,
   invalidTermOfType2
   ) where
import Test.QuickCheck
import DataType
import ComputeTerm (term)
import ArbitrarySig
import GetSignatureInfo (funcSymbolOfType,getAllConstant)

invalidTerm1 :: Int -> Signature -> Gen [Term]
invalidTerm1 n xs = do
    sig <- randomSig xs
    let invalidT = term n sig
    return invalidT

invalidTermOfType1 :: Int -> Type -> Signature -> Gen [Term]
invalidTermOfType1 n t xs = do
    let sig = funcSymbolOfType t xs
        constant = getAllConstant xs
    sig' <- randomSig (Signature (sig++constant))
    let invalidT = term n sig'
    return invalidT

invalidTerm2 :: Int -> Signature -> Gen [Term]
invalidTerm2 n xs = do
    sig <- randomSigTotal xs
    let invalidT = term n sig
    return invalidT

invalidTermOfType2 :: Int -> Type -> Signature -> Gen [Term]
invalidTermOfType2 n t xs = do
    let sig = funcSymbolOfType t xs
        constant = getAllConstant xs
    sig' <- randomSigTotal (Signature (constant++sig))
    let invalidT = term n sig'
    return invalidT

