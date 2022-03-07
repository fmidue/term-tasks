{-# language RecordWildCards #-}

module Main (main) where

import Test.QuickCheck (Gen, generate, suchThat)
import DataType (Error(..), toSignature)
import InvalidTerm (invalidTerms,differentTerms)
import ValidTerm(validTerms)
import System.IO
import Data.List (intercalate)
import Records



withConf :: Certain -> Gen (Bool, [String], [[String]])
withConf Certain{baseConf = Base{..},..} = do
    let (a,b) = termSizeRange
        sig' = toSignature signatures
        correctTerms = validTerms sig' Nothing a b
    correctTerms' <- differentTerms correctTerms (min properTerms (length correctTerms))
    incorrectTerms <- invalidTerms sig' wrongTerms a b `suchThat` (\x->sum (map fst wrongTerms) == sum (map length x))
    let correctTerms'' = map show correctTerms'
        incorrectTerms' = map (map show) incorrectTerms
    return (properTerms > length correctTerms, correctTerms'', incorrectTerms')



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let
      Certain
        { signatures = sig_ex
        , baseConf = Base
          { termSizeRange = (a_ex,b_ex)
          , wrongTerms = e_ex
          , properTerms = number_ex
          }
        }
        = dCertain
    putStrLn "Whenever you just press Enter, the default value will be taken"
    putStrLn $ "Please input the signature you want to use (default is " ++ show (toSignature sig_ex) ++ "): "
    putStrLn "You only need to input name, arguments types and result type. Example: funtion symbol f:A->A->B, written as [\"f\",[\"A\",\"A\"],\"B\"]"
    sig_inp <- getLine
    let sig = (if sig_inp == "" then sig_ex else read sig_inp :: [(String,[String],String)])
    putStr $ "Please input the size range [a,b] of terms (default is [" ++ show a_ex ++ "," ++ show b_ex ++ "] ):\na="
    a_inp <- getLine
    let a = (if a_inp == "" then a_ex else read a_inp :: Int)
    putStr "b="
    b_inp <- getLine
    let b = (if b_inp == "" then b_ex else read b_inp :: Int)
    putStrLn ("Please input the error type and number of incorrect terms in this type (default is " ++ show e_ex ++"):\nError types are: " ++ intercalate ", " (map show [minBound .. maxBound :: Error]) ++ ".")
    e_inp <- getLine
    let e = (if e_inp == "" then e_ex else read e_inp :: [(Int,Error)])
    putStr $ "Please input the number of correct terms you need (default is " ++ show number_ex ++ "):\nnumber="
    number_inp <- getLine
    let number = (if number_inp == "" then number_ex else read number_inp :: Int)
    (tooFewTerms, correctTerms'', incorrectTerms') <-
      generate $ withConf $ Certain
      { signatures = sig
      , baseConf = Base
        {termSizeRange = (a,b)
        , wrongTerms = e
        , properTerms = number }
      }
    if tooFewTerms
    then putStrLn "Unfortunately, there are not enough correct terms. Here are correct terms given to students:" >> mapM_ print correctTerms''
    else putStrLn "Here are correct terms given to students:" >> mapM_ print correctTerms''
    putStrLn "Here are incorrect terms given to students:" >> mapM_ print incorrectTerms'
