module Control where

import Prelude

import Effect (Effect)
import Err (ApplicationError)
import Web.File.Blob (Blob)

newtype OnError = OnError (ApplicationError -> Effect Unit)
newtype StartLoading = StartLoading (Effect Unit)
newtype StopLoading = StopLoading (Effect Unit)
newtype OnSuccess = OnSuccess (Effect Unit)
newtype Handler = Handler (OnError -> StartLoading -> StopLoading -> OnSuccess -> Effect Unit)

instance Semigroup Handler where 
  append (Handler a) (Handler b) = Handler \onError startLoading stopLoading onSuccess -> a onError startLoading stopLoading onSuccess *> b onError startLoading stopLoading onSuccess

instance Monoid Handler where
  mempty = Handler \_ _ _ _ -> pure unit

data SetSubmit
  = TwoButtons String Handler String Handler
  | TextOrAudio (String -> Handler) (Blob -> Handler)