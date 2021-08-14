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

randomTerms :: Int -> Int -> [[Term]] -> Gen [[Term]]
randomTerms _ _ [] = return []
randomTerms num1 num2 ts = do
    let a = head ts
        b = last ts
    a' <- vectorOf num1 (elements a)
    b' <- vectorOf num2 (elements b)
    return [a',b']

main :: IO ()
main = do
    putStr "Do you want to input signature yourself? (Y/N):"
    answer <- getLine
    case answer of
        "Y" -> do
            putStrLn "Please input the signature you want to use: "
            sig <- readLn :: IO [(String,[String],String)]
            putStrLn "Please input the size of terms ([a,b]):"
            putStr "a="
            a <- readLn :: IO Int
            putStr "b="
            b <- readLn :: IO Int
            putStrLn "Please input the symbol you want to present exactly once (only in correct terms): "
            s <- readLn :: IO (Maybe String)
            putStrLn "Please input the number of correct terms (number1) and incorrect terms (number2) you need: "
            putStr "number1="
            number1 <- readLn :: IO Int
            putStr "number2="
            number2 <- readLn :: IO Int
            let sig' = Signature(toSignature sig)
            correctTerms <- generate (vectorOf number1 (elements (validTerms sig' s a b)))
            let correctTerms' = map transTerm correctTerms
            putStr "Do you want to generate incorrect terms with different error types? (Y/N):"
            answer' <- getLine
            case answer' of
                "Y" -> do
                    putStrLn "Please input the error type and number of incorrect terms in this type: "
                    e <- readLn :: IO [(Int,String)]
                    incorrectTerms <- generate(invalidTerms sig' (toErrors e) a b)
                    incorrectTerms' <- generate (vectorOf number2 (elements incorrectTerms))
                    let incorrectTerms'' = map transTerm (catMaybes incorrectTerms')
                    print "Here are correct terms given to students:"
                    print correctTerms'
                    print "Here are incorrect terms given to students:"
                    print incorrectTerms''
                "N" -> do
                    putStrLn "Please input the error type you want to use in incorrect terms: "
                    e <- readLn :: IO String
                    incorrectTerms <- generate (invalidTerms' sig' (toError e) a b)
                    let incorrectTerms' = map transTerm (take number2 incorrectTerms)
                    print "Here are correct terms given to students:"
                    print correctTerms'
                    print "Here are incorrect terms given to students:"
                    print incorrectTerms'
                _ -> main
        "N" -> do
            putStrLn "Please input the symbols you want to use: "
            ls <- readLn :: IO [String]
            putStrLn "Please input the symbols of types you want to use: "
            types <- readLn :: IO [String]
            putStrLn "Please input the size of terms ([a,b]):"
            putStr "a="
            a <- readLn :: IO Int
            putStr "b="
            b <- readLn :: IO Int
            putStrLn "Please input the largest length of a symbol in this signature:"
            l <- readLn :: IO Int
            putStrLn "Please input the number of correct terms (number1) and incorrect terms (number2) you need: "
            putStr "number1="
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
                    terms' <- generate (randomTerms number1 number2 terms)
                    let correctTerms = map transTerm (head terms')
                        incorrectTerms = map transTerm (last terms')
                    print "This is the generated signature:"
                    print sig
                    print "Here are correct terms given to students:"
                    print correctTerms
                    print "Here are incorrect terms given to students:"
                    print incorrectTerms
                "N" -> do
                    putStrLn "Please input the error type you want to use in incorrect terms: "
                    e <- readLn :: IO String
                    (sig,terms) <- generate(allTerms' ls (toType types) (toError e) l a b number1 number2)
                    terms' <- generate (randomTerms number1 number2 terms)
                    let correctTerms = map transTerm (head terms')
                        incorrectTerms = map transTerm (last terms')
                    print "This is the generated signature:"
                    print sig
                    print "Here are correct terms given to students:"
                    print correctTerms
                    print "Here are incorrect terms given to students:"
                    print incorrectTerms
                _ -> main
        _ -> main



