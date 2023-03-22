{-# LANGUAGE NamedFieldPuns #-}

module Tasks.CertainSignature (task) where

import Test.QuickCheck (Gen, suchThat)
import DataType (Term)
import Auxiliary (different)
import InvalidTerm (invalidTerms)
import ValidTerm (validTerms)
import Records

task :: Certain -> Gen (Bool, [Term String], [[Term String]])
task Certain{signatures, root, baseConf = Base{termSizeRange = (a,b), wrongTerms, properTerms}} = do
    let correctTerms = validTerms signatures Nothing a b root
    correctTerms' <- different correctTerms (min properTerms (length correctTerms))
    incorrectTerms <- invalidTerms signatures a b root wrongTerms `suchThat` (\x -> map fst wrongTerms == map length x)
    return (properTerms > length correctTerms, correctTerms', incorrectTerms)
