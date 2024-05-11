module Main where

import Prelude

import App (app)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Deku.Effect as E
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Poll (listen_)
import Model (FromController(..))
import Record (union)
import Record.Studio (sequenceRecord)
import Transitions (answerCompanyName, startFromBeginning)
import Web.DOM.Document (url)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.URL (fromAbsolute, searchParams)
import Web.URL.URLSearchParams as SearchParams
import Yoga.JSON (readJSON_)

main :: Effect Unit
main = do
  setFromController /\ fromController <- E.useState'
  setTypingDiv /\ typingDiv <- E.useState'
  setQuestionHint /\ questionHint <- E.useState Nothing
  setAnswerSpan /\ answerSpan <- E.useState'
  setSubmitIs /\ submitIs <- E.useState'
  setAnswerText /\ answerText <- E.useState ""
  let revealQuestionHint = Just >>> setQuestionHint
  let hideQuestionHint = setQuestionHint Nothing
  void $ listen_ (sequenceRecord { fromController, typingDiv, answerSpan }) \i -> do
    let
      common1 =
        { questionHint: revealQuestionHint
        , typingDiv: i.typingDiv
        , answerSpan: i.answerSpan
        , speed: 50.0
        , setSubmitIs
        , setFromController
        }
    let common2 = common1 `union` { hideQuestionHint, setAnswerText }
    case i.fromController of
      StartFromBeginning -> startFromBeginning common1
      AnswerCompanyName { name } -> answerCompanyName
        $ common2 `union` { companyName: name }
  runInBody $ app
    { questionHint
    , setTypingDiv
    , setAnswerSpan
    , submitIs
    , setAnswerText
    , answerText
    }
  -- allows for a sort of storybook
  params <- window >>= document >>= (toDocument >>> url)
  let
    p = do
      fromAbsolute params
        >>= SearchParams.get "q" <<< searchParams
        >>= readJSON_
  setFromController case fromMaybe 0 p of
    0 -> StartFromBeginning
    1 -> AnswerCompanyName { name: "Apple" }
    _ -> StartFromBeginning
