{-# LANGUAGE NamedFieldPuns, TupleSections, FlexibleContexts, RecordWildCards #-}

module TermTasks.Perturbed where


import Control.Monad.Output (LangM, OutputMonad(indent, refuse), english, german, translate)
import Data.List (elemIndex, nub, sortOn)
import Data.Maybe (fromJust)
import Test.QuickCheck (Gen, shuffle)

import TermTasks.Direct(genInst, verifyBase)
import DataType (Signature(..), Symbol(..), Type(..))
import Records (Certain(..), Perturbed(..), SigInstance)



perturbConfig :: MonadFail Gen => Perturbed -> Gen SigInstance
perturbConfig Perturbed { symbols, types, sigs, baseConf } = do
  theSymbols <- shuffle [1 .. length symbols]
  let remapSymbols s = fromJust (lookup s (zip theSymbols symbols))
  theTypes <- shuffle [1 .. length types]
  let remapTypes t = Type $ fromJust (lookup t (zip theTypes types))
  sigs' <- map snd . sortOn fst <$>
    mapM (\(symbol, (arguments, result)) ->
            shuffle arguments
            >>= \arguments' ->
              let symbol' = remapSymbols symbol
              in return $ (elemIndex symbol' symbols ,) $
                 Symbol { symbol = symbol'
                        , arguments = map remapTypes arguments'
                        , result = remapTypes result }
         )
    (zip [1..] sigs)
  genInst $ Certain {signatures = Signature sigs', baseConf}



verifyPerturbed :: OutputMonad m => Perturbed -> LangM m
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


    | otherwise = verifyBase baseConf
  where
    emptyInput = null symbols || null types
    doubledSymbols = nub symbols /= symbols
    invalidType = let t = map fst sigs
                      r = map snd sigs
                  in
                    any (`notElem` [1..length types]) $ concat $ r : t

