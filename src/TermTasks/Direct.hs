{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module TermTasks.Direct where

import qualified Data.Map                         as M (fromAscList)

import Data.List (nub, sort, (\\))
import Control.Applicative (Alternative)
import Control.Monad (when)
import Control.Monad.Output (
  GenericOutputMonad (indent, itemizeM, latex, refuse, text),
  LangM,
  OutputMonad,
  Rated,
  continueOrAbort,
  english,
  german,
  multipleChoice,
  recoverFrom,
  translate,
  translations,
  )
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.Tuple.Extra (dupe)
import Test.QuickCheck (Gen, shuffle)

import TermTasks.Helpers
import TermTasks.Messages
import DataType (Signature(..), Symbol(..), Type(..), termSize)
import Records (Base(..), Certain(..), SigInstance(..))
import qualified Tasks.CertainSignature as CertainSignature



description :: OutputMonad m => SigInstance -> LangM m
description SigInstance{..} = do
  text1
  indent $ traverse_ (latex . mathifySignature . show) symbols
  text2
  indent $ traverse_ (latex . itemifyTerm) $ zip [1 :: Int ..] terms
  text3
  text4
  pure ()


genInst :: MonadFail Gen => Certain -> Gen SigInstance
genInst c@Certain{..} = do
  (False, correctTerms, incorrectTerms) <- CertainSignature.task c
  (correctness, terms) <- fmap unzip $ shuffle
    $ map (True,) correctTerms ++ map (False,) (concat incorrectTerms)
  let Signature symbols = signatures
      correct = [ i | (i,True) <- zip [1 :: Int ..] correctness]
      moreFeedback = extraFeedback baseConf
      showSolution = printSolution baseConf
  return $ SigInstance { symbols, terms, correct, moreFeedback, showSolution }


verifyInst :: OutputMonad m => SigInstance -> LangM m
verifyInst SigInstance{..}
    | notInRange =
        refuse $ indent $ translate $ do
          english "At least one of the solution indices does not exist."
          german "Mindestens eine der Lösungsindices existiert nicht."
    | not $ null nonTrivialTerms = refuse $ do
        translate $ do
          english "The following terms are not of at least two symbols:"
          german "Die folgenden Terme bestehen nicht wenigstens aus zwei Symbolen:"
        itemizeM $ map (text . show) nonTrivialTerms
        pure ()
    | emptyInput =
        refuse $ indent $ translate $ do
          english "At least one of the given lists is empty."
          german "Mindestens eine der angegebenen Listen ist leer."

    | doubleSolution =
        refuse $ indent $ translate $ do
          english "At least one correct index is given multiple times."
          german "Mindestens ein Lösungsindex wurde mehrfach angegeben."

    | otherwise = pure()
  where
    notInRange = any (`notElem` [1 .. length terms + 1]) correct
    emptyInput = null symbols || null terms
    doubleSolution = nub correct /= correct
    nonTrivialTerms = filter ((< 2) . termSize) terms



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


    | emptyRootList =
        refuse $ indent $ translate $ do
          english "The type list given as possible roots must not be empty."
          german "Die als mögliche Wurzeln angegebene Typ-Liste darf nicht leer sein."


    | incorrectRootType =
        refuse $ indent $ translate $ do
          english "At least one type mentioned as possible root is not returned by any operation."
          german "Mindestens ein Typ, der als mögliche Wurzel genannt wird, wird von keiner Operation zurückgegeben."

    | otherwise = verifyBase baseConf
  where
    usedDefs = definitions signatures
    usedSymbols = map symbol usedDefs
    usedTypes = map name $ concatMap arguments usedDefs
    usedResults = map (name . result) usedDefs

    doubleDef = nub usedSymbols /= usedSymbols
    emptyStrings = any null usedSymbols || any null usedTypes || any null usedResults
    emptyRootList = maybe False null root
    incorrectRootType = maybe False (any (\t -> name t `notElem` usedResults)) root



verifyBase :: OutputMonad m => Base -> LangM m
verifyBase Base{..}
    | negativeAmounts =
        refuse $ indent $ translate $ do
          english "At least one quantity is given as a negative number."
          german "Mindestens eine Anzahl wurde als negative Zahl angegeben."
    | fst termSizeRange < 2 = refuse $ translate $ do
        english "The minimum of 'termSizeRange' has to be at least two."
        german "Das Mimimum der 'termSizeRange' muss mindestens zwei betragen."

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
    duplicateError = let errorTypes = map snd wrongTerms in nub errorTypes /= errorTypes




start :: [Int]
start = []



partialGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
partialGrade SigInstance{..} sol
    | invalidIndex =
        refuse $ indent $ translate $ do
          english "At least one index in the list does not exist."
          german "Mindestens einer der Indices existiert nicht."
    | otherwise = pure ()
  where
    nubSol = nub sol
    invalidIndex = any (`notElem` [1 .. length terms]) nubSol

completeGrade
  :: (Alternative m, OutputMonad m)
  => SigInstance
  -> [Int]
  -> Rated m
completeGrade SigInstance {..} sol = do
  recoverFrom $ assert (not wrongAmount) $
    translate $ do
      english "The amount of indices is correct?"
      german "Die Anzahl der Indices ist richtig?"
  when (wrongAmount && moreFeedback) $
            if diff > 0
              then
                indent $ translate $ do
                  english $ "Your solution contains " ++ displayDiff ++ " additional " ++ eng
                  german $ "Ihre Lösung enthält " ++ displayDiff ++ ger ++ " zu viel."
              else
                indent $ translate $ do
                  english $ "Your solution is missing " ++ displayDiff ++ eng
                  german $ "Ihre Lösung enthält " ++ displayDiff ++ ger ++ " zu wenig."
  when (showSolution || not wrongAmount) $ do
    recoverFrom $ assert (not wrongSolution) $
      translate $ do
        english "Your solution is correct?"
        german "Ihre Lösung ist richtig?"
    when (wrongSolution && moreFeedback) $ indent $ do
            translate $ do
              english "These incorrect terms are part of your solution: "
              german "Diese Terme aus Ihrer Lösung sind falsch: "
            itemizeM $ map (latex . show) badTerms
            pure ()
    pure ()
  let what = translations $ do
        english "terms"
        german "Terme"
      solution = if showSolution then Just (show correct) else Nothing
      matching = M.fromAscList $ map
        (second (`elem` correct) . dupe)
        [1 .. length terms]
  x <- multipleChoice what solution matching sol
  pure x
  where
    assert = continueOrAbort showSolution
    wrongAmount = diff /= 0
    diff =  length nubSol - length correct
    displayDiff = show (abs diff)
    (eng, ger) =
      if abs diff == 1
      then (" index."," Index")
      else (" indices."," Indices")
    nubSol = nub sol
    wrongSolution = sort nubSol /= sort correct
    badTerms = map ((terms !!) . subtract 1) $ nubSol \\ correct
