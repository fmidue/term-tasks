{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}

module TermTasks.Direct where


import Data.List (nub)
import Control.Monad.Output(LangM, OutputMonad(code, indent, refuse), english, german, translate)
import Test.QuickCheck (Gen, shuffle)

import TermTasks.Helpers
import TermTasks.Messages
import DataType (Signature(..), Symbol(..), Type(..))
import Records (Base(..), Certain(..), SigInstance(..))
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

    | otherwise = verifyBase baseConf
  where
    usedDefs = definitions signatures
    usedSymbols = map symbol usedDefs
    usedTypes = map name $ concatMap arguments usedDefs
    usedResults = map (name . result) usedDefs

    doubleDef = nub usedSymbols /= usedSymbols
    emptyStrings = any null usedSymbols || any null usedTypes || any null usedResults



verifyBase :: OutputMonad m => Base -> LangM m
verifyBase Base{..}
    | negativeAmounts =
        refuse $ indent $ translate $ do
          english "At least one quantity is given as a negative number."
          german "Mindestens eine Anzahl wurde als negative Zahl angegeben."


    | invalidInterval =
        refuse $ indent $ translate $ do
          english "The interval of the term lengths is invalid."
          german "Das Intervall für die Termlänge ist ungültig."


    | duplicateError =
        refuse $ indent $ translate $ do
          english "At least one error has been entered multiple times with different quantities. (may induce duplicate terms in task)"
          german "Mindestens einer der Fehlertypen wurde mehrfach mit verschiedener Anzahl angegeben. (kann in doppelten Termen in der Aufgabe resultieren)"


    | otherwise = pure()
  where
    (lower,upper) = termSizeRange
    negativeAmounts = any (<0) $ lower : upper : properTerms : map fst wrongTerms
    invalidInterval = lower > upper
    duplicateError = let errorTypes = map snd wrongTerms in nub errorTypes == errorTypes




start :: [Int]
start = []



partialGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
completeGrade _ _ = pure()
