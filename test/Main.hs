module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import Test.HUnit
import Control.Exception (SomeException)

import Crafty.Parse

-- Utilities

testFileName :: String
testFileName = "test.scm"

parse :: String -> Either SomeException Datum
parse = Crafty.Parse.read testFileName

assertRight :: Show a => Either a b -> IO b
assertRight e = case e of
    Left error' -> assertFailure $ show error'
    Right value -> return value

parse' :: String -> IO Datum
parse' = assertRight . parse

-- Tests

-- Booleans

testParseShortTrue :: Test
testParseShortTrue = TestLabel "Short true" . TestCase $ do
    result <- parse' "#t"
    result @?= Boolean True

testParseShortFalse :: Test
testParseShortFalse = TestLabel "Short false" . TestCase $ do
    result <- parse' "#f"
    result @?= Boolean False

testParseLongTrue :: Test
testParseLongTrue = TestLabel "Long true" . TestCase $ do
    result <- parse' "#true"
    result @?= Boolean True

testParseLongFalse :: Test
testParseLongFalse = TestLabel "Long false" . TestCase $ do
    result <- parse' "#false"
    result @?= Boolean False

booleanTests :: Test
booleanTests = TestLabel "Booleans" $ TestList [
    testParseShortTrue,
    testParseShortFalse,
    testParseLongTrue,
    testParseLongFalse
    ]

-- Identifiers

testParseIdentifier :: Test
testParseIdentifier = TestLabel "Identifier" . TestCase $ do
    result <- parse' "identifier"
    result @?= Symbol "identifier"

testParsePipedIdentifier :: Test
testParsePipedIdentifier = TestLabel "Piped identifier" . TestCase $ do
    result <- parse' "|an IDENTIFIER \x0;|"
    result @?= Symbol "an IDENTIFIER \NUL"

identifierTests :: Test
identifierTests = TestLabel "Identifiers" $ TestList [
    testParseIdentifier,
    testParsePipedIdentifier
    ]

-- Numbers

testParseInteger :: Test
testParseInteger = TestLabel "Parse integer" . TestCase $ do
    result <- parse' "10"
    result @?= Number (Real (Rational (Integer 10)))

testParseDouble :: Test
testParseDouble = TestLabel "Parse double" . TestCase $ do
    result <- parse' "12.34"
    result @?= Number (Real (Rational (Double 12.34)))

testParseRatio :: Test
testParseRatio = TestLabel "Parse ratio" . TestCase $ do
    result <- parse' "1/2"
    result @?= Number (Real (Rational (Ratio 1 2)))

testParseComplex :: Test
testParseComplex = TestLabel "Parse complex" . TestCase $ do
    result <- parse' "1+2i"
    result @?= Number (Rectangular (Rational (Integer 1)) (Rational (Integer 2)))

numberTests :: Test
numberTests = TestLabel "Numbers" $ TestList [
    testParseInteger,
    testParseDouble,
    testParseRatio,
    testParseComplex
    ]

-- All tests

allTests :: Test
allTests = TestList [identifierTests, numberTests, booleanTests]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if failures counts > 0 then exitFailure else exitSuccess
