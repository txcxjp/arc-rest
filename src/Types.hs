{-# LANGUAGE DeriveGeneric #-}
-- allows to write Map and HashMap as lists
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Result (..),
    CommandRequest (..),
    SetupRequest (..),
    SpeakRequest (..),
  )
where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Data (Proxy (Proxy))
import Data.Swagger (HasProperties (properties), HasRequired (required), HasType (type_), NamedSchema (NamedSchema), SwaggerType (SwaggerObject), ToSchema (declareNamedSchema), declareSchemaRef)
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
    commandRequestDevice :: Maybe String
  }

instance FromJSON CommandRequest where
  parseJSON = withObject "CommandRequest" $ \v ->
    CommandRequest
      <$> v
      .: fromString "command"
      <*> v
      .: fromString "device"

instance ToSchema CommandRequest where
  declareNamedSchema _ = do
    strSchema <- declareSchemaRef (Proxy :: Proxy String)
    let c = mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("command", strSchema),
                 ("device", strSchema)
               ]
          & required .~ ["command"]
    return $
      NamedSchema (Just "CommandRequest") $ c