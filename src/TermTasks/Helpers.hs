module TermTasks.Helpers where

import Data.List.Extra (replace)
import DataType (Term)


mathifySignature :: String -> String
mathifySignature s = open
  ++ replace " : " (withMathit " : ")
      (replace " -> " (withMathit " \\to ") $
        replace " x " (withMathit " \\times ") s)
  ++ close
  where
    open = "\\mathit{"
    close = "}"
    withMathit snip = close ++ snip ++ open

itemifyTerm :: (Int, Term String) -> String
itemifyTerm (i,t) = show i ++ ".\\," ++ "\\mathit{" ++ show t ++ "}"
