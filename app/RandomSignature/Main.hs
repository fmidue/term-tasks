module Main (main) where

import Test.QuickCheck (generate)
import DataType (Error(..))
import System.IO
import Data.List (intercalate)
import Records
import qualified Tasks.RandomSignature as RandomSignature

main ::IO ()
main = do
    hSetBuffering stdout NoBuffering
    let
      Random
        { symbols = ls_ex
        , types = types_ex
        , maxArgs = l_ex
        , baseConf = Base
          { termSizeRange = (a_ex,b_ex)
          , wrongTerms = e_ex
          , properTerms = number_ex
          }
        }
        = dRandom
    putStrLn "Whenever you just press Enter, the default value will be taken"
    putStrLn ("Please input the symbols you want to use (default is " ++ show ls_ex ++ "):\nExample: if you want f(x,y), you need to input [\"f\",\"x\",\"y\"]")
    ls_inp <- getLine
    let ls = (if ls_inp == "" then ls_ex else read ls_inp :: [String])
    putStrLn ("Please input the type names of types you want to use (default is " ++ show types_ex ++ "):\nExample: the type name of Type \"A\" is \"A\"")
    types_inp <- getLine
    let theTypes = (if types_inp == "" then types_ex else read types_inp :: [String])
    putStr $ "Please input the size range [a,b] of terms (default is [" ++ show a_ex ++ "," ++ show b_ex ++ "] ):\na="
    a_inp <- getLine
    let a = (if a_inp == "" then a_ex else read a_inp :: Int)
    putStr "b="
    b_inp <- getLine
    let b = (if b_inp == "" then b_ex else read b_inp :: Int)
    putStr $ "Please input the maximum number of arguments of a function symbol in this signature (default is " ++ show l_ex ++ "):\nl="
    l_inp <- getLine
    let l = (if l_inp == "" then l_ex else read l_inp :: Int)
    putStrLn ("Please input the error type and number of incorrect terms in this type (default is " ++ show e_ex ++"):\nError types are: " ++ intercalate ", " (map show [minBound .. maxBound :: Error]) ++ ".")
    e_inp <- getLine
    let e = (if e_inp == "" then e_ex else read e_inp :: [(Int,Error)])
    putStr $ "Please input the number of correct terms you need (default is " ++ show number_ex ++ "):\nnumber="
    number_inp <- getLine
    let number = (if number_inp == "" then number_ex else read number_inp :: Int)
    (sig, correctTerms', incorrectTerms) <-
      generate $ RandomSignature.task $ Random
      { symbols = ls
      , types = theTypes
      , maxArgs = l
      , baseConf = Base
        { termSizeRange = (a,b)
        , wrongTerms = e
        , properTerms = number
        }
      }
    putStrLn "Here are function symbols and constants in the generated signature:"
    print sig
    putStrLn "Here are correct terms given to students:"
    mapM_ print correctTerms'
    putStrLn "Here are incorrect terms given to students:"
    mapM_ print incorrectTerms
