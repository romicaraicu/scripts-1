{-# LANGUAGE OverloadedStrings #-}

module Main where

import Engine
import Models
import System.Environment (getArgs)
import Web.Scotty

main :: IO ()
main = do
  [mode, path] <- getArgs
  results <- detectScripts path >>= executeScripts
  scotty 3000 $ get "/status" $ json results
