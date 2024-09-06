{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import qualified ForthTest as F
import qualified CountingTest as C

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} C.specs
