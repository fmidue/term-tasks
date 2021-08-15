module User where

import Test.QuickCheck
import DataType
import AllTerm (allTerms)
import InvalidTerm (invalidTerms,differentTerms)
import ValidTerm(validTerms)
import Data.List (intercalate)

toSignature :: [(String,[String],String)] -> [Symbol]
toSignature = map (\(s,ts,r)->Symbol s (toType ts) (Type r))

toType :: [String] -> [Type]
toType = map Type

transTerm :: Term -> String
transTerm (Term x []) = x
transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

randomTerms :: Int -> Int -> ([Term],[Term]) -> Gen ([Term],[Term])
randomTerms num1 num2 (a,b) = do
    a' <- differentTerms a num1
    b' <- differentTerms b num2
    return (a',b')

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
    putStr "Please input the number of correct terms (number1) and incorrect terms (number2) you need:\nnumber1="
    number1 <- readLn :: IO Int
    putStr "number2="
    number2 <- readLn :: IO Int
    let sig' = Signature(toSignature sig)
        correctTerms = validTerms sig' Nothing a b
    correctTerms' <-generate (differentTerms correctTerms (min number1 (length correctTerms)))
    incorrectTerms <- generate(invalidTerms sig' e a b)
    incorrectTerms' <- generate (differentTerms incorrectTerms (min number2 (length incorrectTerms)))
    let correctTerms'' = map transTerm correctTerms'
        incorrectTerms'' = map transTerm incorrectTerms'
    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms'')
    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms'')

randomSignature ::IO ()
randomSignature = do
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
    putStr "Please input the number of correct terms (number1) and incorrect terms (number2) you need:\nnumber1="
    number1 <- readLn :: IO Int
    putStr "number2="
    number2 <- readLn :: IO Int
    (sig,terms) <- generate(allTerms ls (toType types) e l a b number1 number2)
    (correctTerms,incorrectTerms) <- generate (randomTerms number1 number2 terms)
    let correctTerms' = map transTerm correctTerms
        incorrectTerms' = map transTerm incorrectTerms
    putStrLn ("This is the generated signature:\n" ++ show sig)
    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms')
    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')

