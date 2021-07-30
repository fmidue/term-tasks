module DealWithTerm (
   termSymbols,
   termSymbols',
   termForm
   ) where

import DataType
import Data.List (intercalate)

termSymbols :: Term -> [String]
termSymbols (Term x xs) = x : termSymbols' xs

termSymbols' :: [Term] -> [String]
termSymbols' = concatMap termSymbols


