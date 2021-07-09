module OneTerm (
  oneValidTerm
  )where

import Test.QuickCheck
import GetSignatureInfo (getAllSameType)
import DataType

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig@(Signature fs) = do
  oneF <- elements fs
  termList <- arbTermList sig (arguments oneF)
  return (Term (funcName oneF) termList)

arbTermList :: Signature -> [Type] -> Gen [Term]
arbTermList sig = mapM (arbTerm sig)

arbTerm :: Signature -> Type -> Gen Term
arbTerm sig t = do
  let list = getAllSameType sig t
  one <- elements list
  termList <- arbTermList sig (arguments one)
  return (Term (funcName one) termList)




