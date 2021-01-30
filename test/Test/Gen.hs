module Test.Gen
  ( genDay,
    genUTCTime,
  )
where

import Data.Time (Day, UTCTime (UTCTime))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

-- | Gen Day with year of Gen.enum (-2020) 2020 and dayOfYear of Gen.enum 1 366.
genDay :: Gen Day
genDay = fromOrdinalDate <$> genYear <*> genDayOfYear
  where
    genYear = Gen.enum (-2020) 2020
    -- If it's not leap year, fromOrdinalDate will make 366 to 365.
    genDayOfYear = Gen.enum 1 (365 + 1)

-- | Gen UTCTime with day of genDay and dayTime of Gen.enum 0 86400.
--
-- We ignore the leap-second for convenience since there's no easy to handle it.
genUTCTime :: Gen UTCTime
genUTCTime = UTCTime <$> genDay <*> genDayTime
  where
    -- We ignore leap-seconds here due to its complexity.
    genDayTime = Gen.enum 0 86400
