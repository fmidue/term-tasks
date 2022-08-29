{-# LANGUAGE NamedFieldPuns, TupleSections, FlexibleContexts #-}

module Autotool.Perturbed where


import Control.Monad.Output (LangM, OutputMonad, indent, code)
import Data.List (sortOn, elemIndex)
import Data.Maybe (fromJust)
import Test.QuickCheck (Gen, shuffle)

import Autotool.Direct(genInst)
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
verifyPerturbed _
    | True = pure()
    | otherwise = pure()
