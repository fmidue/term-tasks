
module TermTasks.Helpers (
  mathifySignature,
  haskellStyleSignature,
  itemifyTerm,
  haskellStyleTerm,
  inMathit,
  ) where

import Data.List (intercalate)
import Data.List.Extra (dropEnd, notNull, replace, takeEnd)
import DataType (Symbol(..), Term(..), Type(..))


mathifySignature :: String -> String
mathifySignature s = open
  ++ replace " : " (around " : ")
      (replace " -> " (around " \\to ") $
        replace " x " (around " \\times ") s)
  ++ close
  where
    around snip = close ++ snip ++ open

itemifyTerm :: (Int, String) -> String
itemifyTerm (i,t) = show i ++ ".\\," ++ inMathit t

inMathit :: String -> String
inMathit a = open ++ a ++ close

open :: String
open = "\\mathit{"

close :: String
close = "}"

haskellStyleSignature :: Symbol -> String
haskellStyleSignature (Symbol s args (Type resultName)) =
  (if isInfix s then drop 1 (dropEnd 1 s) else s) ++ " :: " ++
  foldr ((\next -> ((next ++ " -> ") ++)) . name) resultName args

haskellStyleTerm :: Term String -> String
haskellStyleTerm (Term x []) = x
haskellStyleTerm (Term s [a,b])
  | isInfix s = intercalate "\\ "
    [ decideBrackets infixTermNeedsBrackets a
    , drop 1 $ dropEnd 1 s
    , decideBrackets infixTermNeedsBrackets b
    ]
haskellStyleTerm (Term s xs) = s ++ "\\ " ++
  intercalate "\\ " (map (decideBrackets termNeedsBrackets) xs)

isInfix :: String -> Bool
isInfix s = take 1 s == "(" && takeEnd 1 s == ")"

decideBrackets :: (Term String -> Bool) -> Term String -> [Char]
decideBrackets decider t
  | decider t = "(" ++ display ++ ")"
  | otherwise = display
  where
    display = haskellStyleTerm t

termNeedsBrackets :: Term a -> Bool
termNeedsBrackets (Term _ args) = notNull args

infixTermNeedsBrackets :: Term String -> Bool
infixTermNeedsBrackets (Term _ []) = False
infixTermNeedsBrackets (Term s _) = isInfix s
