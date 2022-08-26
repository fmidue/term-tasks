{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Autotool.Direct where

import Test.QuickCheck --(generate, shuffle)
import DataType (Signature(..))
import Records (Certain(..))
import qualified Tasks.CertainSignature as CertainSignature

import Autotool.Helpers
import Autotool.Messages

import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.Output (
  LangM,
  LangM',
  OutputMonad,
  indent,
  code
  )

main :: IO ()
main = pure()
  {-let languageIsEnglish = False
  --
  let Signature symbols = #signatures dCertain
  --
  putStrLn $ text1 languageIsEnglish
  putStrLn $ unlines (map (mathifySignature . show) symbols)
  --
  (False, correctTerms, incorrectTerms) <- generate (CertainSignature.task dCertain)
  --
  (correctness, theTerms) <- fmap unzip $ generate $ shuffle $ map (True,) correctTerms ++ map (False,) (concat incorrectTerms)
  --
  putStrLn $ text2 languageIsEnglish
  putStrLn $ unlines (map itemifyTerm (zip [1 :: Int ..] theTerms))
  putStrLn $ text3 languageIsEnglish
  putStrLn $ text4 languageIsEnglish
  putStrLn "-------------\n"
  putStrLn $ "solution = " ++ show [ i | (i,True) <- zip [1 :: Int ..] correctness ] ++ "\n"
-}






description :: (MonadIO m, OutputMonad m, MonadFail (LangM' m)) => Certain -> LangM m
description c@Certain{..} = do
  let Signature symbols = signatures
  (False, correctTerms, incorrectTerms) <- liftIO $ generate (CertainSignature.task c)
  (_, theTerms) <- liftIO $ fmap unzip $ generate $ shuffle $ map (True,) correctTerms ++ map (False,) (concat incorrectTerms)
  text1
  indent $ code $ unlines $ map (mathifySignature . show) symbols
  text2
  indent $ code $ unlines $ map itemifyTerm (zip [1 :: Int ..] theTerms)
  text3
  text4






verifyStatic :: OutputMonad m => Certain -> LangM m
verifyStatic _
    | True = pure()

    | otherwise = pure()



verifyQuiz :: OutputMonad m => Certain -> LangM m
verifyQuiz _ = pure()



start :: [Int]
start = []



partialGrade :: OutputMonad m => Certain -> [Int] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => Certain -> [Int] -> LangM m
completeGrade _ _ = pure()
