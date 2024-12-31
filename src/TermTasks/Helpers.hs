{-# LANGUAGE FlexibleContexts #-}
module TermTasks.Helpers (
  mathifySignature,
  itemifyTerm,
  inMathit,
  extra,
  ) where

import Data.List.Extra (replace)
import DataType (Term)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  Language,
  LangM,
  OutputCapable,
  translate,
  )
import Data.Map (Map)
import Control.Monad.State (put)


mathifySignature :: String -> String
mathifySignature s = open
  ++ replace " : " (around " : ")
      (replace " -> " (around " \\to ") $
        replace " x " (around " \\times ") s)
  ++ close
  where
    around snip = close ++ snip ++ open

itemifyTerm :: (Int, Term String) -> String
itemifyTerm (i,t) = show i ++ ".\\," ++ inMathit t

inMathit :: Show a => a -> String
inMathit a = open ++ show a ++ close

open :: String
open = "\\mathit{"

close :: String
close = "}"

extra :: OutputCapable m => Maybe (Map Language String) -> LangM m
extra (Just extraMap) = paragraph $ translate $ put extraMap
extra _ = pure ()
