module DealWithTerm (
   termSymbols,
   termForm
   ) where

import DataType
import Data.List (intercalate)

termSymbols :: Term -> [String]
termSymbols (Term x xs) = x : termSymbols' xs

termSymbols' :: [Term] -> [String]
termSymbols' = concatMap termSymbols

termForm :: Term -> String
termForm (Term x []) = x
termForm (Term s xs) = s ++ "(" ++ intercalate "," (map termForm xs) ++ ")"

