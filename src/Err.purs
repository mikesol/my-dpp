module Err where

import Affjax as Affjax
import Data.List.Types (NonEmptyList)
import Foreign (ForeignError)

data ApplicationError
  = CouldNotUploadAudioDueToAffjax Affjax.Error
  | CouldNotParseResponseFromUploadedAudio (NonEmptyList ForeignError)
  | CouldNotUploadAudioDueToServerError String
  | CouldNotUploadAudioDueToUnknownServerError Int String