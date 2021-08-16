module Main where

import Test.QuickCheck
import DataType (Signature(..),Symbol(..),Type(..),Error(..),transTerm,toType)
import AllTerm (allTerms)
import InvalidTerm (differentTerms)

main ::IO ()
main = do
    putStrLn "Please input the symbols you want to use: "
    ls <- readLn :: IO [String]
    putStr "Please input the symbols of types you want to use:\ntype="
    types <- readLn :: IO [String]
    putStr "Please input the size of terms ([a,b]):\na="
    a <- readLn :: IO Int
    putStr "b="
    b <- readLn :: IO Int
    putStr "Please input the largest length of a symbol in this signature:\nl="
    l <- readLn :: IO Int
    putStrLn "Please input the error type and number of incorrect terms in this type: "
    e <- readLn :: IO [(Int,Error)]
    putStr "Please input the number of correct terms (number) and incorrect terms (number2) you need:\nnumber="
    number <- readLn :: IO Int
    (sig,(correctTerms,incorrectTerms)) <- generate(allTerms ls (toType types) e l a b number)
    correctTerms' <- generate (differentTerms correctTerms number)
    let correctTerms'' = map transTerm correctTerms'
        incorrectTerms' = map (map transTerm) incorrectTerms
    putStrLn ("This is the generated signature:\n" ++ show sig)
    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms'')
    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')
