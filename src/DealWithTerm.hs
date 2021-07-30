module DealWithTerm (
   termSymbols,
   termSymbols'
   ) where

import DataType

termSymbols :: Term -> [String]
termSymbols (Term x xs) = x : termSymbols' xs

termSymbols' :: [Term] -> [String]
termSymbols' = concatMap termSymbols


