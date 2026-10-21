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

import TermTasks.Records                (DisplayStyle(..), SigInstance(..))
import TermTasks.Helpers                (haskellStyleTerm, inMathit)



data TermsLabel = TermsLabel


instance RenderMessage FlexForm TermsLabel where
  renderMessage _   ("en":_) _ = "Correct terms: (indicate all)"
  renderMessage _   _        _ = "Korrekte Terme: (alle angeben)"


toNotation :: SigInstance -> [String]
toNotation inst = map (("\\("++) . (++"\\)") . inMathit . howToDisplay) $ terms inst
  where
    howToDisplay = case displayStyle inst of
      MathStyle -> show
      HaskellStyle -> haskellStyleTerm


termsForm :: SigInstance -> Rendered Widget
termsForm = formify @MultipleChoiceSelection Nothing . labeledCheckboxes
  Vertical
  (fieldSettingsLabel TermsLabel)
  . toNotation
