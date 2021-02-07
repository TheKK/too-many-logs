{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Gen
  ( genDay,
    genUTCTime,
    absFile,
    relDir,
    absDir,
    relFile,
  )
where

import Data.Time (Day, UTCTime (UTCTime))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Path

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

relDir :: Gen (Path Rel Dir)
relDir = do
  p <- oneRelDir
  ps <- Gen.list (Range.linear 1 50) oneRelDir
  return $ foldl' (</>) p ps
  where
    oneRelDir = Gen.mapMaybe parseRelDir $ Gen.string (Range.linear 1 50) Gen.alphaNum

absDir :: Gen (Path Abs Dir)
absDir = do
  rel <- relDir
  return $ [absdir|/|] </> rel

relFile :: Gen (Path Rel File)
relFile = do
  dir <- relDir
  file <- Gen.mapMaybe parseRelFile $ Gen.string (Range.linear 1 50) Gen.alphaNum
  return $ dir </> file

absFile :: Gen (Path Abs File)
absFile = do
  rel <- relFile
  return $ [absdir|/|] </> rel
