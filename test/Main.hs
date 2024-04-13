module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import Test.HUnit
import Control.Exception (SomeException)

import Crafty.Parse
import GHC.Stack (HasCallStack)

-- Utilities

testFileName :: String
testFileName = "test.scm"

parse :: String -> Either SomeException Datum
parse = Crafty.Parse.read testFileName

assertRight :: HasCallStack => Show a => Either a b -> IO b
assertRight e = case e of
    Left error' -> assertFailure $ show error'
    Right value -> return value

parse' :: HasCallStack => String -> IO Datum
parse' = assertRight . parse

parsesTo :: HasCallStack => String -> Datum -> Assertion
parsesTo source datum' = parse' source >>= (@?= datum')

-- Tests

-- Booleans

testParseShortTrue :: Test
testParseShortTrue = TestLabel "Short true" . TestCase $
    "#t" `parsesTo` Boolean True

testParseShortFalse :: Test
testParseShortFalse = TestLabel "Short false" . TestCase $
    "#f" `parsesTo` Boolean False

testParseLongTrue :: Test
testParseLongTrue = TestLabel "Long true" . TestCase $
    "#true" `parsesTo` Boolean True

testParseLongFalse :: Test
testParseLongFalse = TestLabel "Long false" . TestCase $
    "#false" `parsesTo` Boolean False

booleanTests :: Test
booleanTests = TestLabel "Booleans" $ TestList [
    testParseShortTrue,
    testParseShortFalse,
    testParseLongTrue,
    testParseLongFalse
    ]

-- Identifiers

testParseIdentifier :: Test
testParseIdentifier = TestLabel "Identifier" . TestCase $
    "identifier" `parsesTo` Symbol "identifier"

testParsePipedIdentifier :: Test
testParsePipedIdentifier = TestLabel "Piped identifier" . TestCase $
    "|an IDENTIFIER \\x0;|" `parsesTo` Symbol "an IDENTIFIER \NUL"

identifierTests :: Test
identifierTests = TestLabel "Identifiers" $ TestList [
    testParseIdentifier,
    testParsePipedIdentifier
    ]

-- Numbers

testParseInteger :: Test
testParseInteger = TestLabel "Parse integer" . TestCase $
    "10" `parsesTo` Number (Real . Rational $ Integer 10)

testParseDouble :: Test
testParseDouble = TestLabel "Parse double" . TestCase $
    "12.34" `parsesTo` Number (Real . Rational $ Double 12.34)

testParseRatio :: Test
testParseRatio = TestLabel "Parse ratio" . TestCase $
    "1/2" `parsesTo` Number (Real . Rational $ Ratio 1 2)

testParseComplex :: Test
testParseComplex = TestLabel "Parse complex" . TestCase $
    "1+2i" `parsesTo` Number (Rectangular (Rational $ Integer 1) (Rational $ Integer 2))

numberTests :: Test
numberTests = TestLabel "Numbers" $ TestList [
    testParseInteger,
    testParseDouble,
    testParseRatio,
    testParseComplex
    ]

-- Characters

testNamedCharacters :: Test
testNamedCharacters = TestLabel "Named characters" . TestList $ map TestCase [
    "#\\alarm"     `parsesTo` Character '\7',
    "#\\backspace" `parsesTo` Character '\8',
    "#\\delete"    `parsesTo` Character '\127',
    "#\\escape"    `parsesTo` Character '\27',
    "#\\newline"   `parsesTo` Character '\n',
    "#\\null"      `parsesTo` Character '\0',
    "#\\return"    `parsesTo` Character '\r',
    "#\\space"     `parsesTo` Character ' ',
    "#\\tab"       `parsesTo` Character '\t'
    ]

testHexCharacters :: Test
testHexCharacters = TestLabel "Hex characters" . TestList $ map TestCase [
    "#\\x03BB" `parsesTo` Character 'λ',
    "#\\x03bb" `parsesTo` Character 'λ'
    ]

testCharacterLiterals :: Test
testCharacterLiterals = TestLabel "Character literals" . TestList $ map TestCase [
    "#\\x" `parsesTo` Character 'x',
    "#\\1" `parsesTo` Character '1'
    ]

characterTests :: Test
characterTests = TestLabel "Numbers" $ TestList [
    testNamedCharacters,
    testHexCharacters,
    testCharacterLiterals
    ]

-- Strings

testEmptyString :: Test
testEmptyString = TestLabel "Empty string" . TestCase $
    "\"\"" `parsesTo` String ""

testEscapedStringCharacters :: Test
testEscapedStringCharacters = TestLabel "Escaped string characters" . TestList $ map TestCase [
    "\"\\a\""   `parsesTo` String "\7",
    "\"\\b\""   `parsesTo` String "\8",
    "\"\\t\""   `parsesTo` String "\t",
    "\"\\n\""   `parsesTo` String "\n",
    "\"\\r\""   `parsesTo` String "\r",
    "\"\\\"\""  `parsesTo` String "\"",
    "\"\\\\\""  `parsesTo` String "\\",
    "\"\\|\""   `parsesTo` String "|",
    "\"\\x0;\"" `parsesTo` String "\NUL"
    ]

testMultiCharacterString ::Test
testMultiCharacterString = TestLabel "Multi-character string" . TestCase $
    "\"abc123 \\|\"" `parsesTo` String "abc123 |"

stringTests :: Test
stringTests = TestLabel "Strings" $ TestList [
    testEmptyString,
    testEscapedStringCharacters,
    testMultiCharacterString]

-- Vectors

testEmptyVector :: Test
testEmptyVector = TestLabel "Empty vector" . TestCase $
    "#()" `parsesTo` Vector []

vectorTests :: Test
vectorTests = TestLabel "Vectors" $ TestList [
    testEmptyVector
    ]

-- All tests

allTests :: Test
allTests = TestList [
    identifierTests,
    numberTests,
    booleanTests,
    characterTests,
    stringTests,
    vectorTests
    ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if failures counts > 0 then exitFailure else exitSuccess
