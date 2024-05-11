module App where

import Prelude

import AnswerContainer (answerContainer)
import Control (SetSubmit)
import Data.Maybe (Maybe, isJust)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, useState, useState')
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Effect (Effect)
import FRP.Poll (Poll)
import QuestionContainer (questionContainer)
import RecordingModal (recordingModal)
import Web.HTML (HTMLDivElement, HTMLSpanElement)

app
  :: { questionHint :: Poll (Maybe String)
     , setAnswerSpan :: HTMLSpanElement -> Effect Unit
     , setTypingDiv :: HTMLDivElement -> Effect Unit
     , submitIs :: Poll SetSubmit
     , setAnswerText :: String -> Effect Unit
     , answerText :: Poll String
     }
  -> Nut
app { questionHint, setTypingDiv, setAnswerSpan, submitIs, setAnswerText, answerText } = Deku.do
  setRecordingModalOpen /\ recordingModalOpen <- useState false
  setMediaRecorder /\ mediaRecorder <- useState'
  D.div
    [ DA.klass_ "flex flex-col md:flex-row h-screen" ]
    [ questionContainer { setTypingDiv, questionHint }
    , answerContainer
        { setAnswerSpan
        , setRecordingModalOpen
        , setMediaRecorder
        , submitIs
        , setAnswerText
        , answerText
        , revealInputSurface: isJust <$> questionHint
        }
    , recordingModal { recordingModalOpen, setRecordingModalOpen, mediaRecorder }
    ]
