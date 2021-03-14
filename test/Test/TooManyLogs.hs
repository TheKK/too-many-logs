{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.TooManyLogs (tests) where

import Hedgehog
  ( Property,
    annotateShow,
    assert,
    checkParallel,
    cover,
    discover,
    forAll,
    property,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Constants as C
import Test.Gen (absFile, genUTCTime)
import TooManyLogs
  ( FilterType (FilterFromFile, FilterInRange),
    Log (..),
    logIsFromFile,
    logIsInTimeRange,
    logMatchFilter,
  )

prop_Log_Ord_onlySomeFieldsAffectComparing :: Property
prop_Log_Ord_onlySomeFieldsAffectComparing = property $ do
  -- Fieds that affect ordering.
  (timeA, timeB) <- forAll . genTwoDifferent $ genUTCTime
  (fileA, fileB) <- forAll . genTwoDifferent $ absFile
  (indexA, indexB) <- forAll . genTwoDifferent $ Gen.word32 $ Range.linear 1 1000

  -- Fieds that won't affect ordering.
  (hostnameA, hostnameB) <- forAll $ genTwoDifferent genHostname
  (idA, idB) <- forAll $ genTwoDifferent genId
  (messageA, messageB) <- forAll $ genTwoDifferent genMessage

  coverDiff "time" timeA timeB
  coverDiff "file" fileA fileB
  coverDiff "index" indexA indexB
  coverDiff "hostname" hostnameA hostnameB
  coverDiff "id" idA idB
  coverDiff "message" messageA messageB

  let logA = Log fileA indexA timeA hostnameA idA messageA
      logB = Log fileB indexB timeB hostnameB idB messageB

  annotateShow logA
  annotateShow logB

  -- Order of field to compare is:
  --   timestamp, fromFile, indexInFile

  -- When all fields are not equal, timestamp decides ordering.
  annotateShow $ timestamp logA
  annotateShow $ timestamp logB
  compare logA logB === (compare `on` timestamp) logA logB

  -- When only timestamp is equal, fromFile decides ordering.
  let logB' = logB {timestamp = timeA}
   in do
        annotateShow $ fromFile logA
        annotateShow $ fromFile logB'
        compare logA logB' === (compare `on` fromFile) logA logB'

  -- When only timestamp and fromFile are equal, indexInFile decides ordering.
  let logB' = logB {timestamp = timeA, fromFile = fileA}
   in do
        annotateShow $ indexInFile logA
        annotateShow $ indexInFile logB'
        compare logA logB' === (compare `on` indexInFile) logA logB'

  -- When only timestamp, fromFile and indexInFile are equal, they're alway equal.
  let logB' = logB {timestamp = timeA, fromFile = fileA, indexInFile = indexA}
   in logA === logB'
  where
    coverDiff name a b = do
      cover 0 (name <> " eq") $ a == b
      cover 30 (name <> " lt") $ a > b
      cover 30 (name <> " gt") $ a < b

    genTwoDifferent g = Gen.filter (uncurry (/=)) $ liftA2 (,) g g

    genHostname = Gen.text (Range.linear 1 20) Gen.unicode
    genId = Gen.text (Range.linear 1 20) Gen.unicode
    genMessage = Gen.text (Range.linear 1 50) Gen.unicode

prop_filterInRange :: Property
prop_filterInRange = property $ do
  ta <- forAll genUTCTime
  tb <- forAll genUTCTime
  logTimeStamp <- forAll genUTCTime

  -- Select begin & end ourselve to increase the coverage of "in range
  let begin = min ta tb
      end = max ta tb
      isInRange = begin <= logTimeStamp && logTimeStamp <= end

  cover 10 "in range" isInRange
  cover 10 "out of range" $ not isInRange

  let log = C.log {timestamp = logTimeStamp}

  logIsInTimeRange (begin, end) log
    === isInRange

prop_filterInRange_alwayOutOfRange :: Property
prop_filterInRange_alwayOutOfRange = property $ do
  ta <- forAll genUTCTime
  tb <- forAll genUTCTime
  logTimeStamp <- forAll genUTCTime

  let begin = max ta tb
      end = min ta tb
      isInRange = begin <= logTimeStamp && logTimeStamp <= end

  cover 100 "out of range" $ not isInRange

prop_logIsFromFile :: Property
prop_logIsFromFile = property $ do
  fileName <- forAll absFile
  let log = C.log {fromFile = fileName}
  assert $ logIsFromFile fileName log

prop_logMatchFilter_isoToFilterInRange :: Property
prop_logMatchFilter_isoToFilterInRange = property $ do
  begin <- forAll genUTCTime
  end <- forAll genUTCTime
  logTimeStamp <- forAll genUTCTime

  let log = C.log {timestamp = logTimeStamp}

  logMatchFilter (FilterInRange (begin, end)) log
    === logIsInTimeRange (begin, end) log

prop_logMatchFilter_isoToFilterFromFile :: Property
prop_logMatchFilter_isoToFilterFromFile = property $ do
  fileName <- forAll absFile
  let log = C.log {fromFile = fileName}
  logMatchFilter (FilterFromFile fileName) log === logIsFromFile fileName log

tests :: IO Bool
tests = checkParallel $$(discover)
