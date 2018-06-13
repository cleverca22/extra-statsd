{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Types where

import           Data.Aeson (FromJSON)
import           GHC.Generics (Generic)

data Variables = Variables {
    srcadr :: String
  , offset :: String
  , refid :: String
  , stratum :: String
  , reach :: String
  , jitter :: String
  , delay :: String
  , pmode :: String
  , flash :: String
  , keyid :: String
  } deriving (Show, FromJSON, Generic)
