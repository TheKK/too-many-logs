{-# LANGUAGE LambdaCase #-}

module TooManyLogs
  ( Log (..),
    FilterType (..),
    logIsInTimeRange,
    logMatchFilter,
  )
where

import Data.Time (UTCTime)

-- | The log we love and hate.
data Log = Log
  { timestamp :: UTCTime,
    hostname :: Text,
    identifier :: Text,
    message :: Text
  }

data FilterType = FilterInRange (UTCTime, UTCTime)

logIsInTimeRange :: (UTCTime, UTCTime) -> Log -> Bool
logIsInTimeRange (from, to) log = from <= t && t <= to
  where
    t = timestamp log

logMatchFilter :: FilterType -> Log -> Bool
logMatchFilter = \case
  FilterInRange range -> logIsInTimeRange range
