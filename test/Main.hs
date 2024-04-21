module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import Test.HUnit

import Test.Crafty.Datum
import Test.Crafty.Parse

allTests :: Test
allTests = TestList [
    datumTests,
    parseTests
    ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if failures counts > 0 || errors counts > 0 then exitFailure else exitSuccess
