module RecordingModal where

import Prelude

import Deku.Control (text_)
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.SVG as DS
import Deku.DOM.SVG.Attributes as DSA
import Effect (Effect)
import FRP.Poll (Poll)
import Media (MediaRecorder)
import Media as MR

recordingModal
  :: { mediaRecorder :: Poll MediaRecorder
     , recordingModalOpen :: Poll Boolean
     , setRecordingModalOpen :: Boolean -> Effect Unit
     }
  -> Nut
recordingModal { recordingModalOpen, setRecordingModalOpen, mediaRecorder } = do
  let
    transitionClass = recordingModalOpen <#> \isOpen ->
      if isOpen then "ease-out duration-300 opacity-100 translate-y-0 sm:scale-100"
      else "ease-in duration-200 opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"

    pointerEventsTransitionClass = recordingModalOpen <#> \isOpen ->
      if isOpen then "pointer-events-auto"
      else "pointer-events-none"
    backdropTransitionClass = recordingModalOpen <#> \isOpen ->
      if isOpen then "ease-out duration-300 opacity-100 pointer-events-auto"
      else "ease-in duration-200 opacity-0 pointer-events-none"

  fixed
    [ D.div
        [ DA.klass $ backdropTransitionClass <#> (_ <> " fixed inset-0 bg-gray-500 bg-opacity-75")
        ]
        []
    , D.div
        [ DA.klass $ pointerEventsTransitionClass <#> (_ <> " fixed inset-0 z-10 w-screen overflow-y-auto") ]
        [ D.div
            [ DA.klass_ "flex h-full items-end justify-center p-4 text-center sm:items-center sm:p-0" ]
            [ D.div
                [ DA.klass $ transitionClass <#>
                    ( _ <>
                        " relative transform flex flex-col h-full sm:w-1/2 w-full rounded-lg bg-white px-4 pb-4 pt-5 text-left shadow-xl"
                    )
                ]
                [ D.div [ DA.klass_ "flex flex-row w-full justify-end" ]
                    [ D.div_
                        [ D.div
                            [ DA.klass_ "mx-auto flex h-8 w-8 items-center justify-center rounded-full bg-green-100"
                            ]
                            [ DS.svg
                                [ DA.klass_ "w-4 h-4 animate-pulse"
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
                        ]
                    ]
                , D.div
                    [ DA.klass_ "grow mt-3 sm:mt-5"
                    ]
                    [ D.blockquote
                        [ DA.klass_ "mt-2 border-l-4 pl-2 border-gray-200 italic text-gray-400"
                        ]
                        [ text_ "What's your company's name?"
                        ]
                    , D.div
                        [ DA.klass_ "mt-2 text-gray-400"
                        ]
                        [ text_ "Speak your response."
                        ]
                    ]
                , D.div
                    [ DA.klass_ "mt-5 sm:mt-6 flex flex-row space-x-2"
                    ]
                    [ D.div [ DA.klass_ "grow" ]
                        [ D.button
                            [ DA.klass_
                                "inline-flex w-full justify-center rounded-md bg-white px-3 py-2 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 sm:col-start-1 sm:mt-0"
                            , DL.runOn_ DL.click (setRecordingModalOpen false)
                            ]
                            [ text_ "Cancel"
                            ]
                        ]
                    , D.div [ DA.klass_ "grow" ]
                        [ D.button
                            [ DA.klass_
                                "inline-flex w-full justify-center rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
                            , DL.runOn DL.click $ mediaRecorder <#> \mr -> do
                                setRecordingModalOpen false
                                MR.stop mr
                            ]
                            [ text_ "Submit"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
