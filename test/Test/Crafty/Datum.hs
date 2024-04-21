module Test.Crafty.Datum (datumTests) where

import Prelude hiding (Rational, Real, Integer)
import Test.HUnit
import Test.Crafty.Util

import Crafty.Datum

testNumberFromInteger :: Test
testNumberFromInteger = testCase "Number from integer" $ do
    let one = 1 :: Number
    one @?= 1
    assert $ isExact one
    one + one @?= 2
    one * 2 @?= 2

numericTests :: Test
numericTests = testCases "Numeric" [
    testNumberFromInteger
    ]

datumTests :: Test
datumTests = testCases "Datum" [
    numericTests
    ]
