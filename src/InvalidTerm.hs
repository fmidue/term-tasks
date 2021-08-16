module InvalidTerm (
  invalidTerms,
  differentTerms
)where

import Test.QuickCheck
import DataType (Signature(..),Term(..),Error(..))
import ArbitrarySig (swapArgOrder,oneMoreArg,oneLessArg,oneDiffType,wrongSymbol,wrongSymbol')
import ValidTerm (validTerms)

newSignature :: Signature -> Error -> Gen (Maybe(Signature,String))
newSignature sig SWAP = swapArgOrder sig
newSignature sig ONEMORE = fmap Just (oneMoreArg sig)
newSignature sig ONELESS = oneLessArg sig
newSignature sig TYPE = oneDiffType sig
newSignature sig SYMBOL = fmap Just (wrongSymbol sig)
newSignature sig SYMBOLTYPE = fmap Just (wrongSymbol' sig)

originalSymbol :: String -> Term -> Term
originalSymbol s' (Term s ts)
  | s == s' = Term (init s) ts
  | otherwise = Term s (map (originalSymbol s') ts)

differentTerms :: [Term] -> Int -> Gen [Term]
differentTerms _ 0 = return []
differentTerms ts n = do
    nextTerm <- differentTerms ts (n-1)
    t <- elements ts `suchThat` (`notElem` nextTerm)
    return (t:nextTerm)

invalidTerms :: Signature -> [(Int,Error)] -> Int -> Int -> Gen [[Term]]
invalidTerms _ [] _ _ = return []
invalidTerms sig ((n,e):ls) a b = do
  terms <- invalidTerms' sig e a b
  terms' <- differentTerms terms (min n (length terms))
  nextTerm <- invalidTerms sig ls a b
  return (terms':nextTerm)

invalidTerms' :: Signature -> Error -> Int-> Int -> Gen [Term]
invalidTerms' sig e a b = do
    new <- newSignature sig e
    case new of
      Nothing -> return []
      Just (newSig,s) -> do
        let terms = validTerms newSig (Just s) a b
            terms' = map (originalSymbol s) terms
        return terms'