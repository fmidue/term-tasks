module CertainSignature where

import Test.QuickCheck
import DataType
import InvalidTerm (invalidTerms,differentTerms)
import ValidTerm(validTerms)

toSignature :: [(String,[String],String)] -> [Symbol]
toSignature = map (\(s,ts,r)->Symbol s (toType ts) (Type r))

certainSignature :: IO ()
certainSignature = do
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
    incorrectTerms <- generate(invalidTerms sig' e a b)
    let correctTerms'' = map transTerm correctTerms'
        incorrectTerms' = map (map transTerm) incorrectTerms
    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms'')
    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')
