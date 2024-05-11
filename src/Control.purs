module Control where

import Prelude

import Effect (Effect)
import Err (ApplicationError)
import Web.File.Blob (Blob)

newtype OnError =  OnError (ApplicationError -> Effect Unit)
newtype OnLoading = OnLoading (Effect Unit)
newtype OnSuccess = OnSuccess (Effect Unit)
newtype Handler = Handler (OnError -> OnLoading -> OnSuccess -> Effect Unit)

data SetSubmit
  = OneButton String Handler
  | TwoButtons String Handler String Handler
  | TextOrAudio (String -> Handler) (Blob -> Handler)