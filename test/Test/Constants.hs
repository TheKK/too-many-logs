{-# LANGUAGE OverloadedStrings #-}

module Test.Constants
  ( log,
  )
where

import Data.Time (UTCTime (UTCTime))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import TooManyLogs (Log (Log))

log :: Log
log = Log timestamp "hostname" "id" "msg"
  where
    timestamp = UTCTime day dayTime
    day = fromOrdinalDate 2020 87
    dayTime = 18
