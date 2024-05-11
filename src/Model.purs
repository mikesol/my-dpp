module Model where

import Data.Either (Either)
import Web.File.Blob (Blob)

newtype TextOrVoice = TextOrVoice (Either String Blob)

-- data ToController
--   = AskCompanyName
--   | AskCompanyNameAgain
--   | VerifyCompanyName { name :: String }
--   | DescribeProduct { name :: String }
--   | OfferToStartNow { name :: String, description :: String }
--   | ComeBackLater { name :: String, description :: String, url :: String }
--   | StartingNow { name :: String, description :: String, url :: String }

data FromController
  = StartFromBeginning
  | AnswerCompanyName { name :: String }
  -- | AnswerVerifiedCompanyName { name :: String, isCorrect :: Boolean }
  -- | AnswerDescribedProduct { name :: String, description :: TextOrVoice }
  -- | AnswerWillStartNow { name :: String, description :: String, isStartingNow :: Boolean }