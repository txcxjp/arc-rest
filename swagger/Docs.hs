{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Lens
import Data.Aeson (encode, toJSON)
import Data.Swagger
import Lib as MyLib
import Servant (Proxy (..))
import Servant.Swagger
import System.Environment (getArgs)
import Utils (lbsToString)

exportSwagger :: String -> IO ()
exportSwagger filepath = do
  writeFile filepath $
    lbsToString $
      encode $
        toJSON $
          ( toSwagger (Proxy :: Proxy API)
              & info . title .~ "userapi"
              & info . version .~ "1.0"
              & info . description ?~ "This is an API for the Users service"
              & info . license ?~ "MIT"
              & host ?~ "example.com"
              & applyTagsFor setupOperation ["Setup" & description ?~ "you must run this at first."]
              & applyTagsFor resetOperation ["Reset" & description ?~ "re-login and reload available devices.Needed to execute `setup` api in advance."]
          )
  where
    setupOperation :: Traversal' Swagger Operation
    -- myOp = subOperations (Proxy :: Proxy SetupType) (Proxy :: Proxy API)
    setupOperation = subOperations (Proxy :: Proxy SetupType) (Proxy :: Proxy API)

    resetOperation :: Traversal' Swagger Operation
    resetOperation = subOperations (Proxy :: Proxy ResetType) (Proxy :: Proxy API)

-- :: (IsSubAPI sub api, HasSwagger sub) => Proxy sub -> Proxy api -> Traversal' Swagger Operation
-- c :: MyType

-- a = toSwagger

main :: IO ()
main = do
  args <- getArgs
  if null args
    then return ()
    else exportSwagger (head args)