module Main where

import Test.QuickCheck
import DataType (Error(..),transTerm,toType)
import AllTerm (allTerms)
import InvalidTerm (differentTerms)
import System.IO

main ::IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Please input the symbols you want to use: "
    ls <- readLn :: IO [String]
    putStrLn "Please input the type names of types you want to use:"
    types <- readLn :: IO [String]
    putStr "Please input the size of terms ([a,b]):\na="
    a <- readLn :: IO Int
    putStr "b="
    b <- readLn :: IO Int
    putStr "Please input the largest length of a function symbol in this signature:\nl="
    l <- readLn :: IO Int
    putStrLn "Please input the error type and number of incorrect terms in this type: "
    e <- readLn :: IO [(Int,Error)]
    putStr "Please input the number of correct terms you need:\nnumber="
    number <- readLn :: IO Int
    (sig,(correctTerms,incorrectTerms)) <- generate(allTerms ls (toType types) e l a b number)
    correctTerms' <- generate (differentTerms correctTerms number)
    let correctTerms'' = map transTerm correctTerms'
        incorrectTerms' = map (map transTerm) incorrectTerms
    putStrLn ("This is the generated signature:\n" ++ show sig)
    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms'')
    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')
