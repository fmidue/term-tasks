taskName: HaskellTerms
validation: Validate
=============================================

module Global where


import TermTasks.Records (
  SigInstance(..),
  )


type Submission = [Int]
type TaskData = SigInstance

=============================================

module TaskSettings where


import Control.OutputCapable.Blocks
import TermTasks.Direct                 (verifyCertain)
import TermTasks.Records (
  Certain(..),
  Base(..),
  DisplayStyle(..),
  )
import TermTasks.DataType (
  Error(..),
  Signature,
  toSignature,
  )

import qualified Data.Map as M

task :: Certain
task = makeConfig $ toSignature
  [ ("blank", [], "Picture")
  , ("circle", ["Double"], "Picture")
  , ("rectangle", ["Double", "Double"], "Picture")
  , ("rotated", ["Double", "Picture"], "Picture")
  -- escaping '&' due to MathJax encoding problem
  , ("(\\&)", ["Picture", "Picture"], "Picture")
  , ("5", [], "Double")
  , ("7", [], "Double")
  ]


makeConfig :: Signature -> Certain
makeConfig sig = Certain {
  signatures = sig,
  root = Nothing,
  baseConf = Base {
    termSizeRange = ( 6, 10 ),
    wrongTerms = [(1, Swap), (1, TypeChange), (1, OneMore), (1, OneLess)],
    properTerms = 5,
    style = HaskellStyle,
    earlyNumberOfAnswersFeedback = True,
    extraFeedback = True,
    printSolution = True,
    extraText = Static (M.fromList [
      (English, "Note that the details here differ slightly from those in the Modellierung slides,"
                ++ " as does the notation regarding parentheses and the specification of multiple arguments."),
      (German, "Beachten Sie, dass die Details hier etwas anders sind als in den Modellierung-Folien,"
               ++ " ebenso die abweichende Notation hinsichtlich Klammerung und Angabe mehrerer Argumente.")]
    )
  }
}


validateSettings :: OutputCapable m => LangM m
validateSettings = verifyCertain task

=============================================

{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module TaskData where

import Control.Monad.Random             (MonadRandom)
import Data.String.Interpolate          (i)
import FlexTask.GenUtil                 (fromGen)
import FlexTask.Form                    (Rendered, Widget)
import TermTasks.Direct                 (genInst)
import TermTasks.Form                   (termsForm)

import Global                           (TaskData)
import TaskSettings                     (task)


getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = fromGen $ do
    inst <- genInst task
    pure (inst, checkers, termsForm inst)


checkers :: String
checkers = [i|

{-\# Language ApplicativeDo \#-}
{-\# Language RecordWildCards \#-}

module Check where


import Control.OutputCapable.Blocks
import TermTasks.Direct                 (completeGrade, partialGrade)

import Global                           (Submission, TaskData)


checkSyntax
  :: OutputCapable m
  => TaskData
  -> Submission
  -> LangM m
checkSyntax = partialGrade


checkSemantics
  :: OutputCapable m
  => a
  -> TaskData
  -> Submission
  -> Rated m
checkSemantics _ = completeGrade

|]

=============================================

{-# LANGUAGE ApplicativeDo #-}
{-# Language RecordWildCards #-}

module Description (description) where


import qualified TermTasks.Direct       as D

import Control.OutputCapable.Blocks     (LangM, OutputCapable)

import Global                           (TaskData)


description :: OutputCapable m => a -> TaskData -> LangM m
description _ = D.description False

=============================================

module Parse (parseSubmission) where


import Data.Functor            ((<&>))
import FlexTask.Parser  (formParser, getAnswers, parseInfallibly)

import Global                  (Submission)



parseSubmission ::
  Applicative m
  => String
  -> m Submission
parseSubmission input = parseInfallibly formParser input <&> getAnswers

