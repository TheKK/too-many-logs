{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog.Main (defaultMain)
import qualified Test.TooManyLogs

main :: IO ()
main =
  defaultMain
    [ Test.TooManyLogs.tests
    ]
