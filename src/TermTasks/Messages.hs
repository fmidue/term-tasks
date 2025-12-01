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
    german "Entscheiden Sie für die folgenden Terme, ob es sich um korrekte Terme im Sinne der oben gegebenen Deklarationen handelt:"
    english "Decide for the following terms whether or not they are correct terms according to the declarations given above:"

text3 :: OutputCapable m => LangM m
text3 = do
  paragraph $ translate $ do
    german "Geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die genau alle korrekten Terme enthält."
    english "State your answer by giving a list of numbers, indicating exactly all correct terms."
  paragraph $ translate $ do
    german "Zum Beispiel würde [1, 2] bedeuten, dass nur die Terme 1. und 2. der gegebenen korrekt sind."
    english "For example, [1, 2] would mean that only terms 1. and 2. of the given ones are correct."
  pure ()
