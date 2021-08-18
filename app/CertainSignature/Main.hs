module Main (main) where

import Test.QuickCheck (generate, suchThat)
import DataType (Error(..),transTerm,transSignature,toSignature)
import InvalidTerm (invalidTerms,differentTerms)
import ValidTerm(validTerms)
import System.IO
import Data.List (intercalate)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Whenever you just press Enter, the default value will be taken"
    let sig_ex = [("x",[],"A"),("y",[],"B"),("z",[],"C"),("f",["A","A"],"B"),("g",["A","B"],"C"),("h",["A","B","C"],"D")]
    putStrLn $ "Please input the signature you want to use (default is [" ++ intercalate "," (transSignature (toSignature sig_ex)) ++ "]): "
    putStrLn "You only need to input name, arguments types and result type. Example: funtion symbol f:A->A->B, written as [\"f\",[\"A\",\"A\"],\"B\"]"
    sig_inp <- getLine
    let sig = (if sig_inp == "" then sig_ex else read sig_inp :: [(String,[String],String)])
        a_ex = 1
        b_ex = 10
    putStr $ "Please input the size range [a,b] of terms (default is [" ++ show a_ex ++ "," ++ show b_ex ++ "] ):\na="
    a_inp <- getLine
    let a = (if a_inp == "" then a_ex else read a_inp :: Int)
    putStr "b="
    b_inp <- getLine
    let b = (if b_inp == "" then b_ex else read b_inp :: Int)
        e_ex = [(5,SWAP)]
    putStrLn ("Please input the error type and number of incorrect terms in this type (default is " ++ show e_ex ++"):\nError types are: " ++ intercalate ", " (map show [minBound .. maxBound :: Error]) ++ ".")
    e_inp <- getLine
    let e = (if e_inp == "" then e_ex else read e_inp :: [(Int,Error)])
        number_ex = 5
    putStr $ "Please input the number of correct terms you need (default is " ++ show number_ex ++ "):\nnumber="
    number_inp <- getLine
    let number = (if number_inp == "" then number_ex else read number_inp :: Int)
    let sig' = toSignature sig
        correctTerms = validTerms sig' Nothing a b
    correctTerms' <-generate (differentTerms correctTerms (min number (length correctTerms)))
    incorrectTerms <- generate(invalidTerms sig' e a b `suchThat` (\x->sum (map fst e) == sum (map length x)))
    let correctTerms'' = map transTerm correctTerms'
        incorrectTerms' = map (map transTerm) incorrectTerms
    if number > length correctTerms
    then putStrLn "Unfortunately, there are not enough correct terms. Here are correct terms given to students:" >> mapM_ print correctTerms''
    else putStrLn "Here are correct terms given to students:" >> mapM_ print correctTerms''
    putStrLn "Here are incorrect terms given to students:" >> mapM_ print incorrectTerms'
