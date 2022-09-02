{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}

module TermTasks.Direct where


import Control.Monad.Output(LangM, OutputMonad, indent, code)
import Test.QuickCheck (Gen, shuffle)

import TermTasks.Helpers
import TermTasks.Messages
import DataType (Signature(..))
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
verifyInst _
    | True = pure()

    | otherwise = pure()



verifyCertain :: OutputMonad m => Certain -> LangM m
verifyCertain _ = pure()



start :: [Int]
start = []



partialGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SigInstance -> [Int] -> LangM m
completeGrade _ _ = pure()
