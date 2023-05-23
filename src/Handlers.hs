{-# LANGUAGE ScopedTypeVariables #-}

module Handlers
  ( setupHandler,
    commandHandler,
    speakHandler,
    resetHandler
  )
where

import Control.Exception (SomeException)
import Control.Monad.Catch as M (catch)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Servant (Handler)
import System.Environment (setEnv)
import System.Process (readProcessWithExitCode)
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
    proc,
    readProcess,
  )
import Types
  ( CommandRequest (..),
    Result (Result),
    SetupRequest (alexa, amazon, email, language, mfaSecret, password),
    SpeakRequest (content, device),
  )
import Utils

commandHandler :: CommandRequest -> Handler Result
commandHandler req = do
  do
    ( do
        (retExitCode, stdout, stderr) <-
          readProcess
            ( proc
                "/usr/lib/alexa-remote-control/alexa_remote_control.sh"
                (mkArgs (commandRequestDevice req) ++ mkCommand (command req))
            )
        let exitCodeInt = case retExitCode of
              ExitSuccess -> 0
              ExitFailure i -> i
        return $ Result exitCodeInt (lbsToString stdout ++ lbsToString stderr)
      )
    `M.catch` ( \(e :: SomeException) -> do
                  return $ Result 1 (show e)
              )
  where
    mkArgs args =
      case args of
        Just device_ ->
          ["-d", device_]
        _ -> []

    mkCommand c
      | isPrefixOf "textcommand:" c
          || isPrefixOf "playmusic:" c
          || isPrefixOf "speak:" c
          || isPrefixOf "vol:" c
          || elem
            c
            [ "weather",
              "traffic",
              "flashbriefing",
              "goodmorning",
              "singasong",
              "tellstory",
              "pause",
              "play",
              "next",
              "prev",
              "fwd",
              "rwd",
              "shuffle",
              "repeat"
            ] =
          ["-e", c]
      | otherwise = []

setupHandler :: SetupRequest -> Handler Result
setupHandler req = do
  liftIO $ setEnv "AMAZON" (amazon req)
  liftIO $ setEnv "ALEXA" (alexa req)
  liftIO $ setEnv "LANGUAGE" (language req)
  liftIO $ setEnv "EMAIL" (email req)
  liftIO $ setEnv "PASSWORD" (password req)
  liftIO $ setEnv "MFA_SECRET" (mfaSecret req)
  (retExitCode, stdout, stderr) <- liftIO $ do
    readProcessWithExitCode "printenv" [] ""
  let exitCodeInt = case retExitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
  return $ Result exitCodeInt (stdout ++ stderr)

speakHandler :: SpeakRequest -> Handler Result
speakHandler req =
  do
    ( do
        let args =
              ( case device req of
                  Just device_ ->
                    ["-d", device_]
                  _ -> []
              )
                ++ ["-e", "speak:" ++ content req]
        (retExitCode, stdout, stderr) <-
          liftIO $
            readProcess
              (proc "/usr/lib/alexa-remote-control/alexa_remote_control.sh" args)
        let exitCodeInt = case retExitCode of
              ExitSuccess -> 0
              ExitFailure i -> i
        return $ Result exitCodeInt (lbsToString stdout ++ lbsToString stderr)
      )
    `M.catch` ( \(e :: SomeException) -> do
                  return $ Result 1 (show e)
              )

resetHandler :: Handler Result
resetHandler = do
  (retExitCode, stdout, stderr) <- liftIO $ readProcess (proc "/usr/lib/alexa-remote-control/alexa_remote_control.sh" ["-l"])
  exitCodeInt <- case retExitCode of
    ExitSuccess -> do
      (retExitCode, stdout, stderr) <- liftIO $ readProcess (proc "/usr/lib/alexa-remote-control/alexa_remote_control.sh" ["-login"])
      return $ case retExitCode of
        ExitSuccess -> 0
        ExitFailure i -> i
    ExitFailure i -> return i

  return $ Result exitCodeInt (lbsToString stdout ++ lbsToString stderr)
