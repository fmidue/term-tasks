module OneTerm (
  oneValidTerm
  )where

import Test.QuickCheck
import GetSignatureInfo (getAllSameType)
import DataType

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig@(Signature fs) = f sig fs


arbTermList :: Signature -> [Type] -> Gen [Term]
arbTermList sig = mapM (arbTerm sig)

arbTerm :: Signature -> Type -> Gen Term
arbTerm sig t = do
  let list = getAllSameType sig t
  f sig list

f :: Signature -> [FunctionSymbol] -> Gen Term
f sig fs = do
  one <- elements fs
  termList <- arbTermList sig (arguments one)
  return (Term (funcName one) termList)





