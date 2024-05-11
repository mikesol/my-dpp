module QuestionContainer where

import Prelude

import Data.Compactable (compact)
import Data.Maybe (Maybe, isJust)
import Data.Options as Opt
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Self as Self
import FRP.Poll (Poll)
import Typeit (TypeitOptions, typeitGo)

questionContainer
  :: { questionHint :: Poll (Maybe String)
     , typeMe :: Poll (Opt.Options TypeitOptions)
     }
  -> Nut
questionContainer { typeMe, questionHint } = Deku.do
  D.div
    [ DA.klass_ "flex-1 flex flex-col h-full overflow-auto bg-gray-200 p-4"
    ]
    [ D.div
        [ DA.klass_ "p-2 flex-1"
        ]
        [ D.div
            [ DA.contenteditable_ "true"
            , DA.klass_ "text-2xl text-gray-700"
            , Self.self $ typeMe <#> flip typeitGo
            ]
            []
        , D.div
            [ DA.klass $ questionHint <#> \r ->
                (if isJust r then "opacity-100 transition-opacity duration-500 ease-in-out" else "opacity-0 ") <>
                  " text-gray-400 pt-4"
            ]
            [ text (compact questionHint) ]
        ]
    , D.div
        [ DA.klass_ "text-sm text-gray-500 p-2 mt-auto" ]
        [ text_ "Powered by MyDPP" ]
    ]
