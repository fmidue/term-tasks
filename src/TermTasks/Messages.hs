{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
module TermTasks.Messages where

import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate
  )

text1 :: OutputCapable m => LangM m
text1 = paragraph $ translate $ do
    german "Betrachten Sie die folgenden Deklarationen von Konstanten und Operationen:"
    english "Consider the following declarations of constants and operations:"



text2 :: OutputCapable m => LangM m
text2 = paragraph $ translate $ do
    german "Entscheiden Sie für die folgenden Terme, ob sie korrekt im Sinne der oben gegebenen Deklarationen sind:"
    english "Decide for the following terms whether or not they are correct according to the declarations given above:"

text3 :: OutputCapable m => LangM m
text3 = do
  paragraph $ translate $ do
    german "Bitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle korrekten Terme enthält."
    english "Please state your answer by giving a list of numbers, indicating all correct terms."
  paragraph $ translate $ do
    german "Zum Beispiel [1, 2] würde bedeuten, dass nur die Terme 1. und 2. von den angegebenen korrekt sind."
    english "For example, [1, 2] would indicate that only terms 1. and 2. of the given ones are correct."
  pure ()
