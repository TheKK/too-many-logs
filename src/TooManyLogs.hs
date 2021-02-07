{-# LANGUAGE LambdaCase #-}

module TooManyLogs
  ( Log (..),
    FilterType (..),
    logIsInTimeRange,
    logMatchFilter,
    logIsFromFile,
  )
where

import Data.Time (UTCTime)
import Path

-- | The log we love and hate.
data Log = Log
  { fromFile :: Path Abs File,
    timestamp :: UTCTime,
    hostname :: Text,
    identifier :: Text,
    message :: Text
  }

data FilterType
  = FilterInRange (UTCTime, UTCTime)
  | FilterFromFile (Path Abs File)

logIsInTimeRange :: (UTCTime, UTCTime) -> Log -> Bool
logIsInTimeRange (from, to) log = from <= t && t <= to
  where
    t = timestamp log

logIsFromFile :: Path Abs File -> Log -> Bool
logIsFromFile p l = fromFile l == p
{-# INLINE logIsFromFile #-}

logMatchFilter :: FilterType -> Log -> Bool
logMatchFilter = \case
  FilterInRange range -> logIsInTimeRange range
  FilterFromFile f -> logIsFromFile f
