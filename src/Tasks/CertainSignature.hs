{-# LANGUAGE NamedFieldPuns #-}

module Tasks.CertainSignature (task) where

import Test.QuickCheck (Gen, suchThat)
import DataType (Term)
import Auxiliary (different)
import InvalidTerm (invalidTerms)
import ValidTerm (validTerms)
import Records

task :: Certain -> Gen (Bool, [Term], [[Term]])
task Certain{signatures, baseConf = Base{termSizeRange = (a,b), wrongTerms, properTerms}} = do
    let correctTerms = validTerms signatures Nothing a b
    correctTerms' <- different correctTerms (min properTerms (length correctTerms))
    incorrectTerms <- invalidTerms signatures wrongTerms a b `suchThat` (\x->sum (map fst wrongTerms) == sum (map length x))
    return (properTerms > length correctTerms, correctTerms', incorrectTerms)
