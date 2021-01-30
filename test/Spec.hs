{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception.Safe (throwString)
import qualified Test.TooManyLogs

main :: IO ()
main = do
  testsPassed <-
    sequenceA
      [ Test.TooManyLogs.tests
      ]
  unless (and testsPassed) $
    throwString "failed to run tests"
