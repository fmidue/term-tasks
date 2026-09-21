{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module TermTasks.Form (
  termsForm,
  )where


import FlexTask.Form (
  Alignment(Vertical),
  FlexForm,
  MultipleChoiceSelection,
  Rendered,
  Widget,
  formify,
  labeledCheckboxes,
  )
import Yesod (
  RenderMessage(..),
  fieldSettingsLabel,
  )

import TermTasks.Records                (SigInstance(terms))
import TermTasks.Helpers                (inMathit)



data TermsLabel = TermsLabel


instance RenderMessage FlexForm TermsLabel where
  renderMessage _   ("en":_) _ = "Correct terms: (indicate all)"
  renderMessage _   _        _ = "Korrekte Terme: (alle angeben)"


asMathNotation :: SigInstance -> [String]
asMathNotation = map (("\\("++) . (++"\\)") . inMathit) . terms


termsForm :: SigInstance -> Rendered Widget
termsForm = formify @MultipleChoiceSelection Nothing . labeledCheckboxes
  Vertical
  (fieldSettingsLabel TermsLabel)
  . asMathNotation
