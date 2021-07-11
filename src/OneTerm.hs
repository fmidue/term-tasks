module OneTerm (
  oneValidTerm
  )where

import Test.QuickCheck
import GetSignatureInfo (getAllSameType)
import DataType

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig@(Signature fs) = arbTerm sig fs

--arbTermList :: Signature -> [Type] -> Gen [Term]
--arbTermList sig = mapM (arbTerm sig . getAllSameType sig)

--arbTerm :: Signature -> Type -> Gen Term
--arbTerm sig = arbTerm sig . getAllSameType sig

arbTerm :: Signature -> [FunctionSymbol] -> Gen Term
arbTerm sig fs = do
  one <- elements fs
  termList <- mapM (arbTerm sig . getAllSameType sig) (arguments one)
  return (Term (funcName one) termList)





