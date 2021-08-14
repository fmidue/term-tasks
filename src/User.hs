module User where

import Test.QuickCheck
import DataType
import AllTerm (allTerms,allTerms')
import InvalidTerm (invalidTerms,invalidTerms')
import ValidTerm(validTerms)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Bifunctor (second)

toSignature :: [(String,[String],String)] -> [Symbol]
toSignature = map (\(s,ts,r)->Symbol s (toType ts) (Type r))

toType :: [String] -> [Type]
toType = map Type

toError :: String -> Error
toError "SwapUp" = SWAP
toError "ArgumentDuplicate" = DUPLICATE
toError "OneTypeWrong" = TYPE
toError "OneArgumentsMore" = ONEMORE
toError "OneArgumentsLess" = ONELESS
toError _ = error "This will never happen!"

toErrors :: [(Int,String)] -> [(Int,Error)]
toErrors = map (second toError)

transTerm :: Term -> String
transTerm (Term x []) = x
transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

randomTerms :: Int -> Int -> ([Term],[Term]) -> Gen ([Term],[Term])
randomTerms num1 num2 (a,b) = do
    a' <- differentTerms a num1
    b' <- differentTerms b num2
    return (a',b')

differentTerms :: [Term] -> Int -> Gen [Term]
differentTerms _ 0 = return []
differentTerms ts n = do
    nextTerm <- differentTerms ts (n-1)
    t <- elements ts `suchThat` (`notElem` nextTerm)
    return (t:nextTerm)

main :: IO ()
main = do
    putStr "Do you want to input signature yourself? (Y/N):"
    answer <- getLine
    case answer of
        "Y" -> do
            putStrLn "Please input the signature you want to use: "
            sig <- readLn :: IO [(String,[String],String)]
            putStr "Please input the size of terms ([a,b]):\na="
            a <- readLn :: IO Int
            putStr "b="
            b <- readLn :: IO Int
            putStr "Please input the symbol you want to present exactly once (only in correct terms):"
            s <- readLn :: IO (Maybe String)
            putStr "Please input the number of correct terms (number1) and incorrect terms (number2) you need:\nnumber1="
            number1 <- readLn :: IO Int
            putStr "number2="
            number2 <- readLn :: IO Int
            let sig' = Signature(toSignature sig)
            correctTerms <-generate (differentTerms (validTerms sig' s a b) number1)
            let correctTerms' = map transTerm correctTerms
            putStr "Do you want to generate incorrect terms with different error types? (Y/N):"
            answer' <- getLine
            case answer' of
                "Y" -> do
                    putStr "Please input the error type and number of incorrect terms in this type: "
                    e <- readLn :: IO [(Int,String)]
                    incorrectTerms <- generate(invalidTerms sig' (toErrors e) a b)
                    incorrectTerms' <- generate (differentTerms (catMaybes incorrectTerms) number2)
                    let incorrectTerms'' = map transTerm incorrectTerms'
                    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms')
                    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms'')
                "N" -> do
                    putStr "Please input the error type you want to use in incorrect terms: "
                    e <- readLn :: IO String
                    incorrectTerms <- generate(invalidTerms' sig' (toError e) a b)
                    incorrectTerms' <- generate (differentTerms incorrectTerms number2)
                    let incorrectTerms'' = map transTerm incorrectTerms'
                    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms')
                    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms'')
                _ -> main
        "N" -> do
            putStrLn "Please input the symbols you want to use: "
            ls <- readLn :: IO [String]
            putStr "Please input the symbols of types you want to use:\ntype= "
            types <- readLn :: IO [String]
            putStr "Please input the size of terms ([a,b]):\na="
            a <- readLn :: IO Int
            putStr "b="
            b <- readLn :: IO Int
            putStr "Please input the largest length of a symbol in this signature:\nl="
            l <- readLn :: IO Int
            putStrLn "Please input the number of correct terms (number1) and incorrect terms (number2) you need:\nnumber1= "
            number1 <- readLn :: IO Int
            putStr "number2="
            number2 <- readLn :: IO Int
            putStr "Do you want to generate incorrect terms with different error types? (Y/N):"
            answer' <- getLine
            case answer' of
                "Y" -> do
                    putStrLn "Please input the error type and number of incorrect terms in this type: "
                    e <- readLn :: IO [(Int,String)]
                    (sig,terms) <- generate(allTerms ls (toType types) (toErrors e) l a b number1 number2)
                    (correctTerms,incorrectTerms) <- generate (randomTerms number1 number2 terms)
                    let correctTerms' = map transTerm correctTerms
                        incorrectTerms' = map transTerm incorrectTerms
                    putStrLn ("This is the generated signature:\n" ++ show sig)
                    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms')
                    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')
                "N" -> do
                    putStrLn "Please input the error type you want to use in incorrect terms: "
                    e <- readLn :: IO String
                    (sig,terms) <- generate(allTerms' ls (toType types) (toError e) l a b number1 number2)
                    (correctTerms,incorrectTerms) <- generate (randomTerms number1 number2 terms)
                    let correctTerms' = map transTerm correctTerms
                        incorrectTerms' = map transTerm incorrectTerms
                    putStrLn ("This is the generated signature:\n" ++ show sig)
                    putStrLn ("Here are correct terms given to students:\n" ++ show correctTerms')
                    putStrLn ("Here are incorrect terms given to students:\n" ++ show incorrectTerms')
                _ -> main
        _ -> main



