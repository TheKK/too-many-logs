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
    -- | Often this field is used for sorting.
    indexInFile :: Word32,
    timestamp :: UTCTime,
    hostname :: Text,
    identifier :: Text,
    message :: Text
  }
  deriving (Show)

instance Eq Log where
  a == b =
    getAll $
      foldMap
        All
        [ ((==) `on` timestamp) a b,
          ((==) `on` fromFile) a b,
          ((==) `on` indexInFile) a b
        ]

instance Ord Log where
  -- Logically we only care about timestamp, fromFile and indexInFile to decide the
  -- ordering. Other fields should be ignore since sorting on field like message or
  -- hostname makes no sense.
  compare =
    fold
      [ compare `on` timestamp,
        compare `on` fromFile,
        compare `on` indexInFile
      ]

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
