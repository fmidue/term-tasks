module OneTerm where

import Test.QuickCheck
import GetSignatureInfo (getFuncSymbol,giveArgType,getAllSameType,checkConstantSymbol)
import DataType
import Data.Maybe (fromJust)

oneValidTerm :: Signature -> Gen Term
oneValidTerm sig = do
  let f = getFuncSymbol sig
  oneF <- elements f
  let tl = fromJust (giveArgType oneF sig)
  termList <- arbTermList sig tl
  return (Term oneF termList)


arbTermList :: Signature -> [Type] -> Gen [Term]
arbTermList _ [] = return []
arbTermList sig (t:ts) = do
  let list = getAllSameType sig t
  one <- elements list
  if checkConstantSymbol sig one
  then do
    nextList <- arbTermList sig ts
    return (Term one [] : nextList)
  else do
    let tList = fromJust (giveArgType one sig)
    termList <- arbTermList sig tList
    nextList <- arbTermList sig ts
    return (Term one termList : nextList)






