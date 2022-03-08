{-# LANGUAGE NamedFieldPuns #-}

module Tasks.RandomSignature (task) where

import Test.QuickCheck (Gen)
import DataType (Signature, Type(..), Term)
import Auxiliary (different)
import AllTerm (allTerms)
import Records

task :: Random -> Gen (Signature, [Term], [[Term]])
task Random{symbols, types, maxArgs, baseConf = Base{termSizeRange = (a,b), wrongTerms, properTerms}} = do
    (sig,(correctTerms,incorrectTerms)) <- allTerms symbols (map Type types) wrongTerms maxArgs a b properTerms
    correctTerms' <- different correctTerms properTerms
    return (sig, correctTerms', incorrectTerms)
