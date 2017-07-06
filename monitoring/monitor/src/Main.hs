{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when, unless, forM_)
import Engine
import Models
import Opts
import System.Exit (exitWith)
import Web.Scotty

main :: IO ()
main = do
  Opts {..} <- parse
  when optsMonitor $ do
    results <- detectScripts optsPath >>= executeScripts
    scotty 3000 $ get "/status" $ json results
  unless optsMonitor $ do
    reports <- detectScripts optsPath >>= executeScripts
    forM_ reports $ putStr . formatReport
    exitWith $ reportsToExitCode reports
