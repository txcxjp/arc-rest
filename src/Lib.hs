{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Exception.Base (SomeException)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Lazy (toStrict)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (setEnv)
import System.Process (readProcessWithExitCode)
import System.Process.Typed

data Result = Result
  { exitCode :: Int,
    console :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Result

type API =
  "setup" :> ReqBody '[JSON] Lib.SetupRequest :> Put '[JSON] Lib.Result
    :<|> "speak" :> ReqBody '[JSON] Lib.SpeakRequest :> Post '[JSON] Lib.Result
    :<|> "login" :> ReqBody '[JSON] Lib.LoginRequest :> Post '[JSON] Lib.Result

startApp :: IO ()
startApp = do
  System.Environment.setEnv "LANG" "C.UTF-8"
  run 8082 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

data SetupRequest = SetupRequest
  { amazon :: String,
    alexa :: String,
    language :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON SetupRequest

setupHandler :: Lib.SetupRequest -> Handler Lib.Result
setupHandler req = do
  liftIO $ System.Environment.setEnv "AMAZON" (amazon req)
  liftIO $ System.Environment.setEnv "ALEXA" (alexa req)
  liftIO $ System.Environment.setEnv "LANGUAGE" (language req)
  (exitCode, stdout, stderr) <- liftIO $ do
    readProcessWithExitCode "printenv" [] ""
  let exitCodeInt = case exitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
  return $ Result exitCodeInt (stdout ++ stderr)

data SpeakRequest = SpeakRequest
  { content :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON SpeakRequest

speakHandler :: Lib.SpeakRequest -> Handler Lib.Result
speakHandler req = do
  ( do
      -- (exitCode, stdout, stderr) <- liftIO $ do
      --   putStrLn ("speak:" ++ content req)
      --   readProcessWithExitCode "/usr/lib/alexa-remote-control/alexa_remote_control.sh" ["-e", "speak:hello"] ""
      (exitCode, stdout, stderr) <-
        readProcess
          (proc "/usr/lib/alexa-remote-control/alexa_remote_control.sh" ["-e", "speak:テスト"]) -- " ++ content req])
      let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure i -> i
      return $ Result exitCodeInt (Data.Text.unpack $ decodeUtf8 $ toStrict stdout)
    )
    `Control.Monad.Catch.catch` ( \e -> do
                                    liftIO $ putStrLn (show (e :: SomeException))
                                    -- return (ExitFailure 1, "someexception catched : ", show e)
                                    return $ Result 1 (show e)
                                )

data LoginRequest = LoginRequest
  { email :: String,
    password :: String,
    mfaSecret :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON LoginRequest

loginHandler :: Lib.LoginRequest -> Handler Lib.Result
loginHandler req = do
  liftIO $ System.Environment.setEnv "EMAIL" (email req)
  liftIO $ System.Environment.setEnv "PASSWORD" (password req)
  liftIO $ System.Environment.setEnv "MFA_SECRET" (mfaSecret req)

  (exitCode, stdout, stderr) <- liftIO $ do
    readProcessWithExitCode "printenv" [] ""
  let exitCodeInt = case exitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
  return $ Result exitCodeInt (stdout ++ stderr)

server :: Server API
server =
  setupHandler :<|> speakHandler :<|> loginHandler

-- users :: [User]
-- users =
--   [ User 1 "Isaac" "Newton",
--     User 2 "Albert" "Einstein"
--   ]
