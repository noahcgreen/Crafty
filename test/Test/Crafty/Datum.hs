module Test.Crafty.Datum (datumTests) where

import Prelude hiding (Rational, Real, Integer)
import Test.HUnit
import Test.Crafty.Util

import Crafty.Datum

testRationalFromInteger :: Test
testRationalFromInteger = testCase "Rational from integer" $ do
    (1 :: Rational) @?= Integer 1
    (1 :: Rational) @?= Ratio 1 1

numericTests :: Test
numericTests = testCases "Numeric" [
    testRationalFromInteger
    ]

datumTests :: Test
datumTests = testCases "Datum" [
    numericTests
    ]
