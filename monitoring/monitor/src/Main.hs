{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (pack)
import GHC.Generics
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath.Find ((~~?), always, fileName, find)
import System.Process (readProcessWithExitCode)
import Web.Scotty

type Stdout = String

type Stderr = String

data ResultCode
  = Ok
  | Warning
  | Error
  deriving (Show, Generic)

instance ToJSON ResultCode

instance FromJSON ResultCode

type ExecutionResult = (ResultCode, Stdout, Stderr)

data Report = Report
  { path :: FilePath
  , result :: ExecutionResult
  } deriving (Show, Generic)

instance ToJSON Report

instance FromJSON Report

exitToResultCode :: ExitCode -> ResultCode
exitToResultCode ExitSuccess = Ok
exitToResultCode (ExitFailure 2) = Warning
exitToResultCode (ExitFailure _) = Error

detectScripts :: FilePath -> IO [FilePath]
detectScripts = find always (fileName ~~? "*.sh")

runScript :: (FilePath, MVar ExecutionResult) -> IO ()
runScript (path, var) = do
  (rc, out, err) <- readProcessWithExitCode path [] ""
  putMVar var (exitToResultCode rc, out, err)

startScript :: (FilePath, MVar ExecutionResult) -> IO ()
startScript = void . forkIO . runScript

waitScriptFinish :: (FilePath, MVar ExecutionResult) -> IO Report
waitScriptFinish (path, var) = do
  result <- takeMVar var
  return $ Report path result

executeScripts :: [FilePath] -> IO [Report]
executeScripts scripts = do
  xs <- mapM initEmptyMVar scripts
  mapM_ startScript xs
  mapM waitScriptFinish xs
  where
    initEmptyMVar :: FilePath -> IO (FilePath, MVar ExecutionResult)
    initEmptyMVar path = do
      var <- newEmptyMVar :: IO (MVar ExecutionResult)
      return (path, var)

formatResult :: Report -> String
formatResult Report {..} = do
  let (rc, out, err) = result
  case rc of
    Ok -> "[ OK] " ++ path ++ "\n"
    Warning
      | null err -> "[WRN] " ++ path ++ "\n" ++ out
      | null out -> "[WRN] " ++ path ++ "\n" ++ err
      | otherwise -> "[WRN] " ++ path
    Error
      | null err -> "[ERR] " ++ path ++ "\n" ++ out
      | null out -> "[ERR] " ++ path ++ "\n" ++ err
      | otherwise -> "[ERR] " ++ path

main :: IO ()
main = do
  [path] <- getArgs
  results <- detectScripts path >>= executeScripts
  scotty 3000 $ get "/status" $ json results
