module Transitions where

import Prelude

import Actions (defaultSubmitAudio)
import Actions as Actions
import Control (Handler(..), SetSubmit(..))
import Data.Options as Opt
import Effect (Effect)
import Model (FromController(..))
import Rec (shrink)
import Record (union)
import Typeit (TypeitOptions)

type Common1 =
  ( questionHint :: String -> Effect Unit
  , typeMe :: Opt.Options TypeitOptions -> Effect Unit
  , focusAnswerSpan :: Effect Unit
  , clearAnswerSpan :: Effect Unit
  , speed :: Number
  , setSubmitIs :: SetSubmit -> Effect Unit
  , setFromController :: FromController -> Effect Unit
  , apiBase :: String
  )

type Common2 =
  ( hideQuestionHint :: Effect Unit
  , setAnswerText :: String -> Effect Unit
  | Common1
  )

startFromBeginning
  :: { | Common1 }
  -> Effect Unit
startFromBeginning i = do
  Actions.doTypesetting $ shrink i `union`
    { text: [ "Hi!", "Let's create your EU Digital Product Passport.", "What's your company's name?" ]
    , postAction: do
        i.focusAnswerSpan
        i.questionHint "Type your answer, or press the microphone icon to speak your answer."
    }
  let submitText = \companyName -> Handler \_ _ _ _ -> i.setFromController $ AnswerCompanyName { name: companyName }
  let
    submitAudio = defaultSubmitAudio @{ companyName :: String }
      { next: _.companyName >>> { name: _ } >>> AnswerCompanyName >>> i.setFromController
      , url: i.apiBase <> "/company-name"
      }
  i.setSubmitIs $ TextOrAudio submitText submitAudio

testTextOrAudio
  :: { | Common1 }
  -> Effect Unit
testTextOrAudio i = do
  Actions.doTypesetting $ shrink i `union`
    { text: [ "x" ]
    , postAction: do
        i.focusAnswerSpan
        i.questionHint "y"
    }
  i.setSubmitIs $ TextOrAudio mempty mempty

answerCompanyName
  :: { companyName :: String
     | Common2
     }
  -> Effect Unit
answerCompanyName i = do
  i.setAnswerText ""
  i.clearAnswerSpan
  i.hideQuestionHint
  Actions.doTypesetting $ shrink i `union`
    { text: [ "Just to make sure, is the name " <> i.companyName <> " correct?" ]
    , postAction: do
        i.questionHint "Press Yes or No."
    }
  i.setSubmitIs $ TwoButtons "Yes" mempty "No" mempty

testTwoButtons
  :: { | Common1 }
  -> Effect Unit
testTwoButtons i = do
  Actions.doTypesetting $ shrink i `union`
    { text: [ "x" ]
    , postAction: do
        i.focusAnswerSpan
        i.questionHint "y"
    }
  i.setSubmitIs $ TwoButtons "yes" mempty "no" mempty
