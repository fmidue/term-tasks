taskName: HaskellTerms

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
  )
import TermTasks.DataType (
  Error(..),
  Signature,
  toSignature,
  )



task :: Certain
task = makeConfig $ toSignature
  [ ("blank", [], "Picture")
  , ("circle", ["Double"], "Picture")
  , ("rectangle", ["Double", "Double"], "Picture")
  , ("rotated", ["Double", "Picture"], "Picture")
  -- escaping '&' due to MathJax encoding problem
  , ("(\\&)", ["Picture", "Picture"], "Picture")
  , ("d", [], "Double")
  ]


makeConfig :: Signature -> Certain
makeConfig sig = Certain {
  signatures = sig,
  root = Nothing,
  baseConf = Base {
    termSizeRange = ( 6, 10 ),
    wrongTerms = [(1, Swap), (1, TypeChange), (1, OneMore), (1, OneLess)],
    properTerms = 5,
    extraFeedback = True,
    printSolution = True,
    extraText = NoExtraText
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
import FlexTask.YesodConfig             (Rendered, Widget)
import TermTasks.Direct                 (genInst)
import Form                             (termsForm)

import Global                           (TaskData)
import TaskSettings                     (task)


getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = fromGen $ do
    inst <- genInst task
    pure (inst, checkers, termsForm inst)


checkers :: String
checkers = [i|

module Check where


import Control.Applicative              (Alternative)
import Control.OutputCapable.Blocks
import TermTasks.Direct                 (completeGrade)

import Global                           (Submission, TaskData)



checkSyntax
  :: OutputCapable m
  => a
  -> b
  -> LangM m
checkSyntax _ _ = pure ()


checkSemantics
  :: (Alternative m, OutputCapable m)
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

import Control.OutputCapable.Blocks
import Data.Foldable                    (traverse_)
import Data.List                        (intercalate)
import TermTasks.Records                (SigInstance(..))

import Messages                         (text1, text2)
import Global                           (TaskData)
import Helpers (
  haskellStyleSignature,
  itemifyTerm,
  mathifySignature,
  )


description :: OutputCapable m => a -> TaskData -> LangM m
description _ SigInstance{..} = do
  text1
  indent $ traverse_ (latex . mathifySignature . haskellStyleSignature) symbols
  text2
  indent $ traverse_ (latex . itemifyTerm) $ zip [1 :: Int ..] terms
  extra addText
  pure ()

=============================================

module Parse (parseSubmission) where


import Data.Functor            ((<&>))
import FlexTask.Generic.Form   (getAnswers)
import FlexTask.Generic.Parse  (formParser, parseInfallibly)

import Global                  (Submission)



parseSubmission ::
  Applicative m
  => String
  -> m Submission
parseSubmission input = parseInfallibly formParser input <&> getAnswers

=============================================

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Messages where

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

=============================================

module Helpers (
  mathifySignature,
  itemifyTerm,
  inMathit,
  haskellStyleSignature,
  ) where


import Data.List.Extra (
  dropEnd,
  intercalate,
  notNull,
  replace,
  takeEnd
  )
import TermTasks.DataType               (Term(..), Symbol(..), name)



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


inMathit :: Term String -> String
inMathit t = open ++ haskellStyleTerm t ++ close


open :: String
open = "\\mathit{"


close :: String
close = "}"


haskellStyleTerm :: Term String -> String
haskellStyleTerm (Term x []) = x
haskellStyleTerm (Term s [a,b])
  | isInfix s = intercalate "\\ "
    [ decideBrackets (infixTermNeedsBrackets s) a
    , drop 1 $ dropEnd 1 s
    , decideBrackets (infixTermNeedsBrackets s) b
    ]
haskellStyleTerm (Term s xs) = s ++ "\\ " ++
  intercalate "\\ " (map (decideBrackets termNeedsBrackets) xs)


decideBrackets :: (Term String -> Bool) -> Term String -> String
decideBrackets decider t
  | decider t = "(" ++ display ++ ")"
  | otherwise = display
  where display = haskellStyleTerm t


termNeedsBrackets :: Term a -> Bool
termNeedsBrackets (Term _ args) = notNull args


-- Assumption: All defined infix operators are associative
infixTermNeedsBrackets :: String -> Term String -> Bool
infixTermNeedsBrackets _ (Term _ []) = False
infixTermNeedsBrackets lastOp (Term s args) = isInfix s && lastOp /= s


isInfix :: String -> Bool
isInfix s = take 1 s == "(" && takeEnd 1 s == ")"


haskellStyleSignature :: Symbol -> String
haskellStyleSignature (Symbol s args result) = s ++ " :: " ++
  foldr (\next -> ((next ++ " -> ") ++)) (name result) (map name args)


===================================================

{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}

module Form (
  termsForm,
  )where


import FlexTask.FormHelpers             (labeledCheckboxes)
import FlexTask.Generic.Form            (Alignment(Vertical))
import FlexTask.YesodConfig             (FlexForm, Widget, Rendered)
import Yesod (
  RenderMessage(..),
  fieldSettingsLabel,
  )

import TermTasks.Records                (SigInstance(terms))
import Helpers                          (inMathit)



data TermsLabel = TermsLabel


instance RenderMessage FlexForm TermsLabel where
  renderMessage _   ("en":_) _ = "Correct terms: (indicate all)"
  renderMessage _   _        _ = "Korrekte Terme: (alle angeben)"


asMathNotation :: SigInstance -> [String]
asMathNotation = map (("\\("++) . (++"\\)") . inMathit) . terms


termsForm :: SigInstance -> Rendered Widget
termsForm = labeledCheckboxes
  Vertical
  (fieldSettingsLabel TermsLabel)
  . asMathNotation
