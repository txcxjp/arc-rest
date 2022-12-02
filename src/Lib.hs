{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
  )
where

import Control.Exception.Base (SomeException)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Handler,
    JSON,
    Post,
    Proxy (..),
    Put,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import System.Environment (setEnv)
import System.Process (readProcessWithExitCode)
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
    proc,
    readProcess,
  )

data Result = Result
  { exitCode :: Int,
    console :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Result

type API =
  "setup" :> ReqBody '[JSON] Lib.SetupRequest :> Put '[JSON] Lib.Result
    :<|> "speak" :> ReqBody '[JSON] Lib.SpeakRequest :> Post '[JSON] Lib.Result

startApp :: IO ()
startApp = do
  run 80 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

data SetupRequest = SetupRequest
  { amazon :: String,
    alexa :: String,
    language :: String,
    email :: String,
    password :: String,
    mfaSecret :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON SetupRequest

setupHandler :: Lib.SetupRequest -> Handler Lib.Result
setupHandler req = do
  liftIO $ System.Environment.setEnv "AMAZON" (amazon req)
  liftIO $ System.Environment.setEnv "ALEXA" (alexa req)
  liftIO $ System.Environment.setEnv "LANGUAGE" (language req)
  liftIO $ System.Environment.setEnv "EMAIL" (email req)
  liftIO $ System.Environment.setEnv "PASSWORD" (password req)
  liftIO $ System.Environment.setEnv "MFA_SECRET" (mfaSecret req)
  (retExitCode, stdout, stderr) <- liftIO $ do
    readProcessWithExitCode "printenv" [] ""
  let exitCodeInt = case retExitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
  return $ Result exitCodeInt (stdout ++ stderr)

newtype SpeakRequest = SpeakRequest
  { content :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON SpeakRequest

speakHandler :: Lib.SpeakRequest -> Handler Lib.Result
speakHandler req =
  do
    ( do
        (retExitCode, stdout, stderr) <-
          readProcess
            (proc "/usr/lib/alexa-remote-control/alexa_remote_control.sh" ["-d", "フィラデルフィア" , "-e", "speak:" ++ content req])
        let exitCodeInt = case retExitCode of
              ExitSuccess -> 0
              ExitFailure i -> i
        return $ Result exitCodeInt (lbsToString stdout ++ lbsToString stderr)
      )
    `Control.Monad.Catch.catch` ( \(e :: SomeException) -> do
                                    return $ Result 1 (show e)
                                )

server :: Server API
server =
  setupHandler :<|> speakHandler

lbsToString :: BL.ByteString -> String
lbsToString = Data.Text.unpack . decodeUtf8 . toStrict
