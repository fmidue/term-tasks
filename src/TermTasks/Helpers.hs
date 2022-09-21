module TermTasks.Helpers where

import Data.List.Extra (replace)
import DataType (Term)


mathifySignature :: String -> String
mathifySignature s = replace "->" "\\to" (replace "x" "\\times" s)

itemifyTerm :: (Int, Term) -> String
itemifyTerm (i,t) = show i ++ ". " ++ show t
