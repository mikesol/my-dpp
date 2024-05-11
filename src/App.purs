module App where

import Prelude

import AnswerContainer (answerContainer)
import Control (SetSubmit)
import Data.Maybe (Maybe, isJust)
import Data.Options as Opt
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, useState, useState')
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Do as Deku
import Effect (Effect)
import FRP.Poll (Poll)
import QuestionContainer (questionContainer)
import RecordingModal (recordingModal)
import Typeit (TypeitOptions)

app
  :: { questionHint :: Poll (Maybe String)
     , focusAnswerSpan :: Poll Unit
     , clearAnswerSpan :: Poll Unit
     , typeMe :: Poll (Opt.Options TypeitOptions)
     , submitIs :: Poll SetSubmit
     , setAnswerText :: String -> Effect Unit
     , answerText :: Poll String
     }
  -> Nut
app
  { questionHint
  , typeMe
  , focusAnswerSpan
  , clearAnswerSpan
  , submitIs
  , setAnswerText
  , answerText
  } = Deku.do
  setRecordingModalOpen /\ recordingModalOpen <- useState false
  setMediaRecorder /\ mediaRecorder <- useState'
  D.div
    [ DA.klass_ "flex flex-col md:flex-row h-screen" ]
    [ questionContainer { typeMe, questionHint }
    , answerContainer
        { focusAnswerSpan
        , clearAnswerSpan
        , setRecordingModalOpen
        , setMediaRecorder
        , submitIs
        , setAnswerText
        , answerText
        , revealInputSurface: isJust <$> questionHint
        }
    , recordingModal { recordingModalOpen, setRecordingModalOpen, mediaRecorder }
    ]
