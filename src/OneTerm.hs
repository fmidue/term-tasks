module OneTerm (
  oneValidTerm
  )where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig@(Signature fs) = arbTerm sig fs

arbTerm :: Signature -> [FunctionSymbol] -> Gen Term
arbTerm sig fs = do
  one <- elements fs
  termList <- mapM (arbTerm sig . allSameTypes sig) (arguments one)
  return (Term (symbol one) termList)
