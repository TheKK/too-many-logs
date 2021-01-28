module TooManyLogs
  ( Log (..),
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
