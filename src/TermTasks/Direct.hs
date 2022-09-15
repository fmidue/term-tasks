{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}

module TermTasks.Direct where


import Data.List (nub)
import Control.Monad.Output(LangM, OutputMonad, code, english, german, indent, refuse, translate)
import Test.QuickCheck (Gen, shuffle)

import TermTasks.Helpers
import TermTasks.Messages
import DataType (Signature(..), Symbol(..), Type(..))
import Records (Certain(..), SigInstance(..))
import qualified Tasks.CertainSignature as CertainSignature



description :: OutputMonad m => SigInstance -> LangM m
description SigInstance{..} = do
  text1
  indent $ code $ unlines $ map (mathifySignature . show) symbols
  text2
  indent $ code $ unlines $ map itemifyTerm (zip [1 :: Int ..] terms)
  text3
  text4



genInst :: MonadFail Gen => Certain -> Gen SigInstance
genInst c@Certain{..} = do
  (False, correctTerms, incorrectTerms) <- CertainSignature.task c
  (correctness, theTerms) <- fmap unzip $ shuffle $ map (True,) correctTerms ++ map (False,) (concat incorrectTerms)
  let Signature symbols = signatures
      correct = [ i | (i,True) <- zip [1 :: Int ..] correctness]
  return $ SigInstance symbols theTerms correct


verifyInst :: OutputMonad m => SigInstance -> LangM m
verifyInst SigInstance{..}
    | notInRange =
        refuse $ indent $ translate $ do
          english "At least one of the solution indices does not exist."
          german "Mindestens eine der Lösungsindices existiert nicht."


    | emptyInput =
        refuse $ indent $ translate $ do
          english "At least one of the given lists is empty."
          german "Mindestens eine der angegebenen Listen ist leer."

    | otherwise = pure()
  where
    notInRange = any (`notElem` [1 .. length terms + 1]) correct
    emptyInput = null symbols || null terms



verifyCertain :: OutputMonad m => Certain -> LangM m
verifyCertain Certain{..}
    | doubleDef =
        refuse $ indent $ translate $ do
          english "At least one symbol is defined multiple times."
          german "Mindestens eines der Symbole wurde mehrfach definiert."


    | emptyStrings =
        refuse $ indent $ translate $ do
          english "At least one of the given symbols is an empty String."
          german "Mindestens eines der Symbole wurde als leerer String angegeben."

    | otherwise = pure () --verifyBase baseConf
  where
    usedDefs = definitions signatures
    usedSymbols = map symbol usedDefs
    usedTypes = map name $ concatMap arguments usedDefs
    usedResults = map (name . result) usedDefs

    doubleDef = nub usedSymbols == usedSymbols
    emptyStrings = any null usedSymbols || any null usedTypes || any null usedResults



start :: [Int]
start = []



partialGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
completeGrade _ _ = pure()
