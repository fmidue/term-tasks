module InvalidTerm where

import Test.QuickCheck
import DataType
import ArbitrarySig (swapArgOrder,duplicateArg,oneMoreArg,oneLessArg,oneDiffType)
import AllTerm (validTerms,oneValidTerm)
import Data.Maybe (isJust)

oneInvalidTerm :: Signature -> Error -> Int-> Int -> Gen (Maybe Term)
oneInvalidTerm sig e a b = do
    (newSig,s) <- newSignature sig e
    let terms = validTerms newSig (Just s) a b
    term <- oneValidTerm terms
    case term of
      Nothing -> return Nothing
      Just t -> return (Just(originalSymbol s t))

newSignature :: Signature -> Error -> Gen (Signature,String)
newSignature sig SWAP = swapArgOrder sig
newSignature sig DUPLICATE = duplicateArg sig
newSignature sig ONEMORE = oneMoreArg sig
newSignature sig ONELESS = oneLessArg sig
newSignature sig TYPE = oneDiffType sig

originalSymbol :: String -> Term -> Term
originalSymbol s' (Term s ts)
  | s == s' = Term (init s) ts
  | otherwise = Term s (map (originalSymbol s') ts)

invalidTerms :: Signature -> [(Int,Error)] -> Int -> Int -> Gen [Maybe Term]
invalidTerms _ [] _ _ = return []
invalidTerms sig ((0,_):ls) a b = invalidTerms sig ls a b
invalidTerms sig ((n,e):ls) a b = do
  term <- oneInvalidTerm sig e a b `suchThat` isJust
  nextTerm <- invalidTerms sig ((n-1,e):ls) a b
  return (term:nextTerm)

invalidTerms' :: Signature -> Error -> Int-> Int -> Gen [Term]
invalidTerms' sig e a b = do
    (newSig,s) <- newSignature sig e
    let terms = validTerms newSig (Just s) a b
        terms' = map (originalSymbol s) terms
    return terms'