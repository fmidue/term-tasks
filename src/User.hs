module User where

import Test.QuickCheck
import DataType
import AllTerm (allTerms)
import InvalidTerm (invalidTerms,invalidTerms')
import ValidTerm(validTerms)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

toSignature :: [(String,[String],String)] -> [Symbol]
toSignature xs = map (\(s,ts,r)->Symbol s (toType ts) (Type r)) xs

toType :: [String] -> [Type]
toType xs = map Type xs

toError :: String -> Error
toError "SwapUp" = SWAP
toError "ArgumentDuplicate" = DUPLICATE
toError "OneTypeWrong" = TYPE
toError "OneArgumentsMore" = ONEMORE
toError "OneArgumentsLess" = ONELESS
toError _ = error "This will never happen!"

toErrors :: [(Int,String)] -> [(Int,Error)]
toErrors xs = map (\(n,e)-> (n,toError e)) xs

transTerm :: Term -> String
transTerm (Term x []) = x
transTerm (Term s xs) = s ++ "(" ++ intercalate "," (map transTerm xs) ++ ")"

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
            putStrLn "Please input the symbol you want to present exactly once (only in valid terms): "
            s <- readLn :: IO (Maybe String)
            putStrLn "Please input the number of valid terms (number1) and invalid terms (number2) you need: "
            putStr "number1="
            number1 <- readLn :: IO Int
            putStr "number2="
            number2 <- readLn :: IO Int
            let sig' = Signature(toSignature sig)
                validterms = map transTerm (take number1 (validTerms sig' s a b))
            putStr "Do you want to generate invalid terms with different error types? (Y/N):"
            answer' <- getLine
            case answer' of
                "Y" -> do
                    putStrLn "Please input the error type and number of invalid terms in this type: "
                    e <- readLn :: IO [(Int,String)]
                    let e' = toErrors e
                    invalidterms <- generate (invalidTerms sig' e' a b)
                    let invalidterms' = map transTerm (catMaybes invalidterms)
                    print "Here are correct terms given to students:"
                    print validterms
                    print "Here are correct terms given to students:"
                    print invalidterms'
                "N" -> do
                    putStrLn "Please input the error type you want to use in invalid terms: "
                    e <- readLn :: IO String
                    let e' = toError e
                    invalidterms <- generate (invalidTerms' sig' e' a b)
                    let invalidterms' = map transTerm (take number2 invalidterms)
                    print "Here are correct terms given to students:"
                    print validterms
                    print "Here are correct terms given to students:"
                    print invalidterms'
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
            putStrLn "Please input the number of valid terms (number1) and invalid terms (number2) you need: "
            putStr "number1="
            number1 <- readLn :: IO Int
            putStr "number2="
            number2 <- readLn :: IO Int
            putStrLn "Please input the error type you want to use in invalid terms: "
            e <- readLn :: IO String
            let types' = toType types
                e' = toError e
            terms <- generate (allTerms ls types' e' l a b)
            let terms' = [map transTerm (take number1 (head terms)), map transTerm (take number2 (last terms))]
            print terms'
        _ -> main



