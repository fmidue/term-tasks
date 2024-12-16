module TermTasks.Helpers (
  mathifySignature,
  itemifyTerm,
  inMathit,
  ) where

import Data.List.Extra (replace)
import DataType (Term)


mathifySignature :: String -> String
mathifySignature s = wrapDollar $ open
  ++ replace " : " (around " : ")
      (replace " -> " (around " \\to ") $
        replace " x " (around " \\times ") s)
  ++ close
  where
    around snip = close ++ snip ++ open

itemifyTerm :: (Int, Term String) -> String
itemifyTerm (i,t) = show i ++ ".\\," ++ inMathit t

inMathit :: Show a => a -> String
inMathit a = wrapDollar $ open ++ show a ++ close

open :: String
open = "\\mathit{"

close :: String
close = "}"

wrapDollar :: String -> String
wrapDollar s = '$': s ++ "$"
