module AnswerContainer where

import Prelude

import Control (Handler(..), OnError(..), OnLoading(..), OnSuccess(..), SetSubmit(..))
import Control.Monad.ST.Class (liftST)
import Data.Array.ST as STArray
import Data.Foldable (for_)
import Data.MediaType (MediaType(..))
import Data.Tuple (Tuple(..))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (injectElementT)
import Deku.DOM.Listeners as DL
import Deku.DOM.SVG as DS
import Deku.DOM.SVG.Attributes as DSA
import Deku.Hooks (guard, (<#~>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)
import Media (MediaRecorder, getUserMedia, mediaRecorder)
import Media as MR
import Web.DOM.Node (textContent)
import Web.Event.Event as Event
import Web.File.Blob as Blob
import Web.HTML (HTMLSpanElement, window)
import Web.HTML.HTMLSpanElement as HTMLSpanElement
import Web.HTML.Window (navigator)

answerContainer
  :: { setAnswerSpan :: HTMLSpanElement -> Effect Unit
     , setMediaRecorder :: MediaRecorder -> Effect Unit
     , setRecordingModalOpen :: Boolean -> Effect Unit
     , submitIs :: Poll SetSubmit
     , setAnswerText :: String -> Effect Unit
     , answerText :: Poll String
     , revealInputSurface :: Poll Boolean
     }
  -> Nut
answerContainer
  { submitIs
  , setAnswerSpan
  , setRecordingModalOpen
  , setMediaRecorder
  , setAnswerText
  , answerText
  , revealInputSurface
  } =
  Deku.do
    let
      revealSurface hideOnText classes = (Tuple <$> revealInputSurface <*> answerText) <#> \(Tuple rti at) ->
        (if rti then "opacity-100 transition-opacity duration-500 ease-in-out " else "opacity-0 ")
          <> (if at /= "" && hideOnText then "hidden " else "")
          <> classes
    D.div
      [ DA.klass_ "flex flex-col flex-1 p-4 overflow-auto bg-white h-full" ]
      [ D.div [ DA.klass_ "p-2 flex-1" ]
          [ D.span
              [ DA.klass_ "text-2xl text-gray-700 outline-none caret-black"
              , DA.contenteditable_ "true"
              , DL.input_ \e -> for_ (Event.target e >>= HTMLSpanElement.fromEventTarget >>> map HTMLSpanElement.toNode)
                  (textContent >=> setAnswerText)
              , injectElementT (setAnswerSpan :: HTMLSpanElement -> Effect Unit)
              ]
              []
          , D.span
              [ DA.klass $ revealSurface true "text-2xl text-gray-300 outline-none"
              ]
              [ text_ "Type your response here..." ]
          ]
      , submitIs <#~> case _ of
          OneButton txt effect -> mempty
          TwoButtons leftTxt leftButton rightTxt rightButton -> mempty
          TextOrAudio submitText submitAudio -> D.div [ DA.klass $ revealSurface false "flex flex-row justify-around" ]
            [ D.button
                [ DA.klass_
                    "rounded-full shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 px-2 py-2"
                , DL.runOn DL.click $ submitIs <#> \sIs -> do
                    n <- window >>= navigator
                    launchAff_ do
                      stream <- getUserMedia n
                      arr <- liftST $ STArray.new
                      let
                        ondataavailable = \e -> do
                          void $ liftST $ STArray.push e arr
                      let
                        onstop = do
                          o <- liftST $ STArray.freeze arr
                          let blob = Blob.fromArray o $ MediaType "audio/mp3"
                          let Handler handler = submitAudio blob
                          handler (OnError mempty) (OnLoading mempty) $ OnSuccess do
                            setRecordingModalOpen false
                      liftEffect do
                        rec <- mediaRecorder stream ondataavailable onstop
                        MR.start rec
                        _ <- setRecordingModalOpen true
                        setMediaRecorder rec
                ]
                [ DS.svg
                    [ DA.klass_ "w-6 h-6"
                    , DSA.strokeWidth_ "1.5"
                    , DSA.viewBox_ "0 0 24 24"
                    , DSA.stroke_ "currentColor"
                    ]
                    [ DS.path
                        [ DSA.d_
                            "M12 18.75a6 6 0 0 0 6-6v-1.5m-6 7.5a6 6 0 0 1-6-6v-1.5m6 7.5v3.75m-3.75 0h7.5M12 15.75a3 3 0 0 1-3-3V4.5a3 3 0 1 1 6 0v8.25a3 3 0 0 1-3 3Z"
                        , DSA.strokeLinecap_ "round"
                        , DSA.strokeLinejoin_ "round"
                        ]
                        []
                    ]
                ]
            , D.button
                [ DA.klass_
                    "rounded bg-white px-2 py-1 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
                , DL.runOn DL.click $ answerText <#> \t -> do
                    let Handler handler = submitText t
                    handler (OnError mempty) (OnLoading mempty) (OnSuccess mempty)
                ]
                [ text_ "Submit" ]
            ]
      ]
