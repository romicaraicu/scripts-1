{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

type Stdout = String

type Stderr = String

data ResultCode
  = Ok
  | Warning
  | Error
  deriving (Eq, Show, Generic)

instance ToJSON ResultCode

instance FromJSON ResultCode

type ExecutionResult = (ResultCode, Stdout, Stderr)

data Report = Report
  { path :: FilePath
  , result :: ExecutionResult
  } deriving (Show, Generic)

instance ToJSON Report

instance FromJSON Report
