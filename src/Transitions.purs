module Transitions where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (printError)
import Affjax.Web as AW
import Control (Handler(..), OnError(..), OnLoading(..), OnSuccess(..), SetSubmit(..))
import DOM (clearHTML)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Model (FromController(..))
import Rec (shrink)
import Record (union)
import Typeit (typeitGo)
import Typeit as Ti
import Untagged.Union (asOneOf)
import Web.HTML (HTMLDivElement, HTMLSpanElement)
import Web.HTML.HTMLDivElement as HTMLDivElement
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLSpanElement as HTMLSpanElement
import Web.XHR.FormData as FormData
import Yoga.JSON (readJSON)

doTypesetting
  :: { typingDiv :: HTMLDivElement
     , postAction :: Effect Unit
     , text :: Array String
     , speed :: Number
     }
  -> Effect Unit
doTypesetting { typingDiv, postAction, text, speed } = do
  -- setReveal false
  typeitGo (HTMLDivElement.toElement typingDiv)
    ( Ti.strings := asOneOf
        text
        <> Ti.speed := speed
        <> Ti.afterComplete := \i -> do
          Ti.destroy i true
          postAction
    )

type Common1 =
  ( questionHint :: String -> Effect Unit
  , typingDiv :: HTMLDivElement
  , answerSpan :: HTMLSpanElement
  , speed :: Number
  , setSubmitIs :: SetSubmit -> Effect Unit
  , setFromController :: FromController -> Effect Unit
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
  doTypesetting $ shrink i `union`
    { text: [ "Hi!", "Let's create your EU Digital Product Passport.", "What's your company's name?" ]
    , postAction: do
        focus (HTMLSpanElement.toHTMLElement i.answerSpan)
        i.questionHint "Type your answer, or press the microphone icon to speak your answer."
    }
  let submitText = \companyName -> Handler \_ _ _ -> i.setFromController $ AnswerCompanyName { name: companyName }
  let
    submitAudio = \blob -> Handler \(OnError _) (OnLoading _) (OnSuccess onSuccess) -> do
      fd <- FormData.new
      FormData.appendBlob (FormData.EntryName "file") blob Nothing fd
      launchAff_ do
        r <- AW.post RF.string "http://127.0.0.1:5000/company-name" $ Just $ FormData fd
        case r of
          Left e -> liftEffect $ log $ printError e
          Right { body } -> case readJSON body of
            Right (x :: Either String String) -> case x of
              Right companyName -> liftEffect do
                i.setFromController $ AnswerCompanyName { name: companyName }
                onSuccess
              Left error -> liftEffect $ log error
            Left err -> liftEffect $ logShow err
  i.setSubmitIs $ TextOrAudio submitText submitAudio

answerCompanyName
  :: { companyName :: String
     | Common2
     }
  -> Effect Unit
answerCompanyName i = do
  i.setAnswerText ""
  clearHTML $ HTMLSpanElement.toElement i.answerSpan
  i.hideQuestionHint
  doTypesetting $ shrink i `union`
    { text: [ "Just to make sure, is the name " <> i.companyName <> " correct?" ]
    , postAction: do
        i.questionHint "Press Yes or No."
    }