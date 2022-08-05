module Helpers where

import DataType (Term)
import Data.List.Extra (replace)

mathifySignature :: String -> String
mathifySignature s = "$" ++ replace "->" "\\to" (replace "x" "\\times" s) ++ "$\n"

itemifyTerm :: (Int, Term) -> String
itemifyTerm (i,t) = show i ++ ". $" ++ show t ++ "$\n"
