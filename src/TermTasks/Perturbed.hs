{-# LANGUAGE NamedFieldPuns, FlexibleContexts, RecordWildCards #-}

module TermTasks.Perturbed where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (indent, refuse),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  )
import Data.List (elemIndex, nub, sortOn)
import Data.List.Extra (nubOrd)
import Data.Maybe (fromJust)
import Test.QuickCheck (Gen, shuffle)

import TermTasks.Direct(genInst, verifyBase)
import DataType (Signature(..), Symbol(..), Type(..))
import Records (Certain(..), Perturbed(..), SigInstance)



perturbConfig :: Perturbed -> Gen SigInstance
perturbConfig Perturbed { symbols, types, signatures, root, baseConf } = do
  theSymbols <- shuffle [1 .. length symbols]
  let remapSymbols s = fromJust (lookup s (zip theSymbols symbols))
  theTypes <- shuffle [1 .. length types]
  let remapTypes t = Type $ fromJust (lookup t (zip theTypes types))
  signatures' <- map snd . sortOn fst <$>
    mapM (\(symbol, (arguments, result)) ->
            shuffle arguments
            >>= \arguments' ->
              let symbol' = remapSymbols symbol
              in return (elemIndex symbol' symbols ,
                 Symbol { symbol = symbol'
                        , arguments = map remapTypes arguments'
                        , result = remapTypes result }
                 )
         )
    (zip [1..] signatures)
  genInst $ Certain {signatures = Signature signatures', root = map remapTypes <$> root, baseConf}


verifyPerturbed :: OutputCapable m => Perturbed -> LangM m
verifyPerturbed Perturbed{..}
     | emptyInput =
        refuse $ indent $ translate $ do
          english "The list of types and/or the list of symbols is empty."
          german "Die Liste der Typen und/oder die Liste der Symbole ist leer."

    | doubledSymbols =
        refuse $ indent $ translate $ do
          english "At least one symbol was given multiple times."
          german "Mindestens ein Symbol wurde mehrfach angegeben."


    | invalidType =
        refuse $ indent $ translate $ do
          english "At least one given definition uses an invalid type index."
          german "Mindestens eine der Definitionen verwendet einen ungültigen Typen-Index."


    | emptyRootList =
        refuse $ indent $ translate $ do
          english "The type list given as possible roots must not be empty."
          german "Die als mögliche Wurzeln angegebene Typ-Liste darf nicht leer sein."


    | incorrectRootType =
        refuse $ indent $ translate $ do
          english "At least one type index mentioned as possible root is not returned by any operation."
          german "Mindestens ein Typen-Index, der als mögliche Wurzel genannt wird, wird von keiner Operation zurückgegeben."


    | otherwise = verifyBase baseConf
  where
    usedTypes = nubOrd $ concatMap fst signatures
    usedResults = nubOrd $ map snd signatures

    emptyInput = null symbols || null types
    doubledSymbols = nub symbols /= symbols
    invalidType = any (`notElem` [1 .. length types]) $ usedTypes ++ usedResults
    emptyRootList = maybe False null root
    incorrectRootType = maybe False (any (`notElem` usedResults)) root
