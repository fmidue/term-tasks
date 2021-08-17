module Main where

import Test.QuickCheck
import DataType (Signature(..),Symbol(..),Type(..),Error(..),transTerm,toType)
import InvalidTerm (invalidTerms,differentTerms)
import ValidTerm(validTerms)
import AllTerm (theLength,theSum)
import System.IO

toSignature :: [(String,[String],String)] -> [Symbol]
toSignature = map (\(s,ts,r)->Symbol s (toType ts) (Type r))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Please input the signature you want to use: "
    sig <- readLn :: IO [(String,[String],String)]
    putStr "Please input the size of terms ([a,b]):\na="
    a <- readLn :: IO Int
    putStr "b="
    b <- readLn :: IO Int
    putStrLn "Please input the error type and number of incorrect terms in this type: "
    e <- readLn :: IO [(Int, Error)]
    putStr "Please input the number of correct terms you need:\nnumber="
    number <- readLn :: IO Int
    let sig' = Signature(toSignature sig)
        correctTerms = validTerms sig' Nothing a b
    correctTerms' <-generate (differentTerms correctTerms (min number (length correctTerms)))
    incorrectTerms <- generate(invalidTerms sig' e a b `suchThat` (\x->theSum e == theLength x))
    let correctTerms'' = map transTerm correctTerms'
        incorrectTerms' = map (map transTerm) incorrectTerms
    if number > length correctTerms
    then putStrLn ("Unfortunately, there are not enough correct terms. Here are correct terms given to students:\n" ++ show correctTerms'')
    else putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms'')
    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')
