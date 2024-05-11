module Media where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Web.HTML (Navigator)

data MediaStream

foreign import getUserMediaImpl :: Navigator -> Effect (Promise MediaStream)

getUserMedia :: Navigator -> Aff MediaStream
getUserMedia = map toAffE getUserMediaImpl

data MediaRecorder

foreign import mediaRecorder :: MediaStream -> (String -> Effect Unit) -> Effect Unit -> Effect MediaRecorder

foreign import start :: MediaRecorder -> Effect Unit

foreign import stop :: MediaRecorder -> Effect Unit
