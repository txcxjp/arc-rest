{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.Process (readProcessWithExitCode)

data Result = Result
  { exitCode :: Int,
    console :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Lib.Result

data LoginRequest = LoginRequest
  { email :: String,
    password :: String,
    mfaSecret :: String
  }
  deriving (Eq, Show, Generic)

data SpeakRequest = SpeakRequest
  { email :: Maybe String,
    password :: Maybe String,
    mfaSecret :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance FromJSON Lib.LoginRequest

type API =
  "speak" :> Get '[JSON] Lib.Result
    :<|> "login" :> ReqBody '[JSON] Lib.LoginRequest :> Post '[JSON] Lib.Result

startApp :: IO ()
startApp = do
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

speakHandler :: Handler Lib.Result
speakHandler = do
  (exitCode, stdout, stderr) <- liftIO $ do
    readProcessWithExitCode "alexa-remote-control.sh" ["-e speak:"] ""
  let exitCodeInt = case exitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
  return $ Result exitCodeInt (stdout ++ stderr)

loginHandler :: Lib.LoginRequest -> Handler Lib.Result
loginHandler req = do
  liftIO $ setEnv "EMAIL" (email req)
  liftIO $ setEnv "PASSWORD" (password req)
  liftIO $ setEnv "MFA_SECRET" (mfaSecret req)

  (exitCode, stdout, stderr) <- liftIO $ do
    readProcessWithExitCode "printenv" [] ""
  let exitCodeInt = case exitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
  return $ Result exitCodeInt (stdout ++ stderr)

server :: Server API
server =
  speakHandler :<|> loginHandler

-- users :: [User]
-- users =
--   [ User 1 "Isaac" "Newton",
--     User 2 "Albert" "Einstein"
--   ]
