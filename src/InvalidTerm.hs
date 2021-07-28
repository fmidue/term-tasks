module InvalidTerm where

import Test.QuickCheck
import DataType
import ArbitrarySig (swapOrder,duplicateArg,oneMoreArg,oneLessArg,oneDiffType)
import OneTerm (oneValidTerm)
import Data.Maybe (isJust)

oneInvalidTerm :: Signature -> Error -> Int-> Int -> Gen (Maybe Term)
oneInvalidTerm sig e a b = do
    (newSig,s) <- newSignature sig e
    term <- oneValidTerm newSig (Just s) a b
    case term of
        Nothing -> return Nothing
        Just t -> return (Just (originalSymbol s t))

newSignature :: Signature -> Error -> Gen (Signature,String)
newSignature sig SWAP = swapOrder sig
newSignature sig DUPLICATE = duplicateArg sig
newSignature sig ONEMORE = oneMoreArg sig
newSignature sig ONELESS = oneLessArg sig
newSignature sig TYPE = oneDiffType sig

originalSymbol :: String -> Term -> Term
originalSymbol s' (Term s ts)
  | s == s' = Term (take 1 s) ts
  | otherwise = Term s (map (originalSymbol s') ts)

invalidTerms :: Signature -> [(Int,Error)] -> Int -> Int -> Gen [Maybe Term]
invalidTerms _ [] _ _ = return []
invalidTerms sig ((0,_):ls) a b = invalidTerms sig ls a b
invalidTerms sig ((n,e):ls) a b = do
  term <- oneInvalidTerm sig e a b `suchThat` isJust
  nextTerm <- invalidTerms sig ((n-1,e):ls) a b
  return (term:nextTerm)