{-# LANGUAGE TupleSections #-}

module Main (main) where

import Test.QuickCheck (generate, shuffle)
import DataType (Signature(..))
import Records (dCertain, Certain(..))
import qualified Tasks.CertainSignature as CertainSignature

import Helpers
import Messages

main :: IO ()
main = do
  let languageIsEnglish = False
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
