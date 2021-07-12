module OneTerm (
  oneValidTerm
  )where

import Test.QuickCheck
import GetSignatureInfo (allSameTypes)
import DataType

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig@(Signature fs) = arbTerm sig fs

--arbTermList :: Signature -> [Type] -> Gen [Term]
--arbTermList sig = mapM (arbTerm sig . allSameTypes sig)

--arbTerm :: Signature -> Type -> Gen Term
--arbTerm sig = arbTerm sig . allSameTypes sig

arbTerm :: Signature -> [FunctionSymbol] -> Gen Term
arbTerm sig fs = do
  one <- elements fs
  termList <- mapM (arbTerm sig . allSameTypes sig) (arguments one)
  return (Term (symbol one) termList)

--oneValidTerm' :: Signature -> Int -> Int -> Gen Term




