{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    API,
    SetupType,
    SpeakType,
    CommandType,
  )
where

import Handlers
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( JSON,
    Post,
    Proxy (..),
    Put,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Types

type API =
  SetupType
    :<|> SpeakType
    :<|> CommandType

type SetupType = "setup" :> ReqBody '[JSON] SetupRequest :> Put '[JSON] Result

type SpeakType = "speak" :> ReqBody '[JSON] SpeakRequest :> Post '[JSON] Result

type CommandType = "command" :> ReqBody '[JSON] CommandRequest :> Post '[JSON] Result

startApp :: IO ()
startApp = do
  run 80 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  setupHandler :<|> speakHandler :<|> commandHandler
