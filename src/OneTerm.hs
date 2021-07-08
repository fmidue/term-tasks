module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (getAllFunction,getAllSameType,checkConstantSymbol)
import DataType

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig = do
  let f = getAllFunction sig
  oneF <- elements f
  termList <- arbTermList sig (arguments oneF)
  return (Term (funcName oneF) termList)

arbTermList :: Signature -> [Type] -> Gen [Term]
arbTermList sig = mapM (arbTerm sig)

arbTerm :: Signature -> Type -> Gen Term
arbTerm sig t = do
  let list = getAllSameType sig t
  one <- elements list
  if checkConstantSymbol sig one
  then return (Term (funcName one) [])
  else do
    termList <- arbTermList sig (arguments one)
    return (Term (funcName one) termList)




