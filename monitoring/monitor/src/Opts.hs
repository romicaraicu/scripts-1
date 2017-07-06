module Opts
  ( Opts(..)
  , parse
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
  { optsMonitor :: Bool
  , optsPeriod :: Maybe Int
  , optsPath :: FilePath
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> switch
      (  long "monitor"
      <> short 'm'
      <> help "Start monitoring with RESTful API server" )
  <*> option auto
      (  long "period"
      <> showDefault
      <> value (Just 30)
      <> metavar "PERIOD_SECONDS"
      <> help "Defines period with which scripts are getting executed" )
  <*> strOption
      (  long "path"
      <> short 'p'
      <> metavar "PATH"
      <> help "Path where script to be located" )

optsInfo :: ParserInfo Opts
optsInfo = info (optsParser <**> helper)
  (  fullDesc
  <> progDesc "Runs a set of scripts and serves results either to stdout or as RESTful API with --monitor switch" )

parse :: IO Opts
parse = execParser optsInfo
