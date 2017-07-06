{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Engine
  ( detectScripts
  , executeScripts
  , formatReport
  , reportsToExitCode
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Data.List (elem)
import Data.Text.Lazy (pack)
import Models
import System.Exit (ExitCode(..))
import System.FilePath.Find ((~~?), always, fileName, find)
import System.Process (readProcessWithExitCode)

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

formatReport :: Report -> String
formatReport Report {..} = do
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

reportsToExitCode :: [Report] -> ExitCode
reportsToExitCode reports = do
  let xs = map (\ Report {..} -> fst3 result) reports
      hasErrors = Error `elem` xs
      hasWarnings = Warning `elem` xs
  case (hasErrors, hasWarnings) of
    (True, _) -> ExitFailure 1
    (False, True) -> ExitFailure 2
    (False, False) -> ExitSuccess
  where
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x
