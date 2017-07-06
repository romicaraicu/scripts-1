module Opts
  ( Opts(..)
  , parseOpts
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
  { optsMonitor :: Bool
  , optsMonitorPort :: Int
  , optsCheckPeriod :: Int
  , optsPath :: FilePath
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> switch
      (  long "monitor"
      <> short 'm'
      <> help "Start monitoring with RESTful API server" )
  <*> option auto
      (  long "port"
      <> showDefault
      <> value 3000
      <> metavar "MONITOR_PORT"
      <> help "Defines port number RESTful API server is accepting connections on" )
  <*> option auto
      (  long "period"
      <> showDefault
      <> value 30
      <> metavar "CHECK_PERIOD_SECONDS"
      <> help "Defines check period with which scripts are getting executed" )
  <*> strOption
      (  long "path"
      <> short 'p'
      <> metavar "PATH"
      <> help "Path where script to be located" )

optsInfo :: ParserInfo Opts
optsInfo = info (optsParser <**> helper)
  (  fullDesc
  <> progDesc "Runs a set of scripts and aggregates results" )

parseOpts :: IO Opts
parseOpts = execParser optsInfo
