module Messages where

import Data.Bool (bool)

text1 :: Bool -> String
text1 = bool
  "Betrachten Sie die folgenden Deklarationen von Konstanten und Operationen:\n"
  "Consider the following declarations of constants and operations:\n"

text2 :: Bool -> String
text2 = bool
  "Entscheiden Sie fuer die folgenden Terme, ob sie korrekt im Sinne der oben gegebenen Deklarationen sind:\n"
  "Decide for the following terms whether or not they are correct according to the declarations given above:\n"

text3 :: Bool -> String
text3 = bool
  "Bitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle korrekten Terme enthaelt.\n"
  "Please state your answer by giving a list of numbers, indicating all correct terms.\n"

text4 :: Bool -> String
text4 = bool
  "Zum Beispiel [1, 2] wuerde bedeuten, dass nur die Terme 1. und 2. von den angegebenen korrekt sind.\n"
  "For example, [1, 2] would indicate that only terms 1. and 2. of the given ones are correct.\n"
