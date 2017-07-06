{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when, unless, forM_)
import Data.Maybe (fromMaybe)
import Engine
import Models
import Opts
import System.Exit (exitWith)
import Web.Scotty

main :: IO ()
main = do
  Opts {..} <- parseOpts
  when optsMonitor $ do
    results <- detectScripts optsPath >>= executeScripts
    scotty optsMonitorPort $ get "/status" $ json results
  unless optsMonitor $ do
    reports <- detectScripts optsPath >>= executeScripts
    forM_ reports $ putStr . formatReport
    exitWith $ reportsToExitCode reports
