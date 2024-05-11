module Actions where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AW
import Control (Handler(..), OnError(..), OnSuccess(..), StartLoading(..), StopLoading(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Options as Opt
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Err (ApplicationError(..))
import Typeit (TypeitOptions)
import Typeit as Ti
import Untagged.Union (asOneOf)
import Web.File.Blob (Blob)
import Web.XHR.FormData as FormData
import Yoga.JSON (class ReadForeign, readJSON)

doTypesetting
  :: { typeMe :: Opt.Options TypeitOptions -> Effect Unit
     , postAction :: Effect Unit
     , text :: Array String
     , speed :: Number
     }
  -> Effect Unit
doTypesetting { typeMe, postAction, text, speed } = 
  typeMe
    ( Ti.strings := asOneOf text
        <> Ti.speed := speed
        <> Ti.afterComplete := \i -> do
          Ti.destroy i true
          postAction
    )

defaultSubmitAudio
  :: forall @input
   . ReadForeign input
  => { next :: input -> Effect Unit, url :: String }
  -> Blob
  -> Handler
defaultSubmitAudio i blob = Handler
  \(OnError onError) (StartLoading startLoading) (StopLoading stopLoading) (OnSuccess onSuccess) -> do
    launchAff_ $ bracket (liftEffect startLoading) (const $ liftEffect stopLoading) \_ -> do
      fd <- liftEffect FormData.new
      liftEffect $ FormData.appendBlob (FormData.EntryName "file") blob Nothing fd
      r <- AW.post RF.string i.url $ Just $ FormData fd
      case r of
        Left err -> liftEffect $ onError $ CouldNotUploadAudioDueToAffjax err
        Right { status, body } -> case status of
          StatusCode 200 -> case readJSON body of
            Right (input :: input) -> liftEffect do
              i.next input
              onSuccess
            Left err -> liftEffect $ onError $ CouldNotParseResponseFromUploadedAudio err
          StatusCode 500 -> liftEffect $ onError $ CouldNotUploadAudioDueToServerError body
          StatusCode sc -> liftEffect $ onError $ CouldNotUploadAudioDueToUnknownServerError sc body