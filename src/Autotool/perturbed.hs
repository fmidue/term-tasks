{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Autotool.Perturbed where

import Test.QuickCheck (Gen, generate, shuffle)
import DataType (Signature(..), Symbol(..), Type(..))
import Records (Certain(..), Perturbed(..), dPerturbed)
import Data.Maybe (fromJust)
import Data.List (sortOn, elemIndex)
import qualified Tasks.CertainSignature as CertainSignature

import Autotool.Helpers
import Autotool.Messages

perturbConfig :: Perturbed -> Gen Certain
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
  return $ Certain {signatures = Signature sigs', baseConf}

main :: IO ()
main = pure()
{-
  do
  let languageIsEnglish = False
  --
  actualConf@(Certain {signatures = Signature symbols'}) <- generate $ perturbConfig dPerturbed
  --
  putStrLn $ text1 languageIsEnglish
  putStrLn $ unlines (map (mathifySignature . show) symbols')
  --
  (False, correctTerms, incorrectTerms) <- generate (CertainSignature.task actualConf)
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
