{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Constants
  ( log,
  )
where

import Data.Time (UTCTime (UTCTime))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Path (Abs, File, absfile)
import TooManyLogs (Log (Log))

log :: Log
log = Log fromFile indexInFile timestamp "hostname" "id" "msg"
  where
    fromFile = [absfile|/var/log/logs|]
    indexInFile = 11
    timestamp = UTCTime day dayTime
    day = fromOrdinalDate 2020 87
    dayTime = 18
