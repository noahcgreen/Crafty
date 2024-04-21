module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import Test.HUnit
import Test.Crafty.Parse

main :: IO ()
main = do
    counts <- runTestTT parseTests
    if failures counts > 0 || errors counts > 0 then exitFailure else exitSuccess
