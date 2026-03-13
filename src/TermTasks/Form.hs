{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}

module TermTasks.Form (
  inputForm,
  )where


import FlexTask.FormHelpers             (labeledCheckboxes)
import FlexTask.Generic.Form            (Alignment(Vertical))
import FlexTask.YesodConfig             (FlexForm, Widget, Rendered)
import Yesod (
  FieldSettings,
  RenderMessage(..),
  fieldSettingsLabel,
  )

import TermTasks.Records                (SigInstance(terms))
import TermTasks.Helpers                (inMathit)



data TermsLabel = TermsLabel


instance RenderMessage app TermsLabel where
  renderMessage _   ("en":_) _ = "Correct terms: (indicate all)"
  renderMessage _   _        _ = "Korrekte Terme: (alle angeben)"


termsLabel :: FieldSettings FlexForm
termsLabel = fieldSettingsLabel TermsLabel


asMathNotation :: SigInstance -> [String]
asMathNotation = map (("\\("++) . (++"\\)") . inMathit) . terms


inputForm :: SigInstance -> Rendered Widget
inputForm = labeledCheckboxes Vertical termsLabel . asMathNotation
