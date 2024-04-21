module Test.Crafty.Util where

import Test.HUnit

testCase :: String -> Assertion -> Test
testCase label = TestLabel label . TestCase

testCases :: String -> [Test] -> Test
testCases label = TestLabel label . TestList
