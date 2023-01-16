{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Result (..),
    CommandRequest (..),
    SetupRequest (..),
    SpeakRequest (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)

data Result = Result
  { exitCode :: Int,
    console :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Result

instance ToSchema Result

data SetupRequest = SetupRequest
  { amazon :: String,
    alexa :: String,
    language :: String,
    email :: String,
    password :: String,
    mfaSecret :: String
  }
  deriving (Generic)

instance FromJSON SetupRequest

instance ToSchema SetupRequest

data SpeakRequest = SpeakRequest
  { content :: String,
    device :: Maybe String
  }
  deriving (Generic)

instance FromJSON SpeakRequest

instance ToSchema SpeakRequest

data CommandRequest = CommandRequest
  { command :: String,
    devicea :: Maybe String
  }
  deriving (Generic)

instance FromJSON CommandRequest

instance ToSchema CommandRequest