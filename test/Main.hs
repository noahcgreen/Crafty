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

-- Tests

-- Booleans

testParseShortTrue :: Test
testParseShortTrue = TestLabel "Short true" . TestCase $
    parse' "#t" >>= (@?= Boolean True)

testParseShortFalse :: Test
testParseShortFalse = TestLabel "Short false" . TestCase $
    parse' "#f" >>= (@?= Boolean False)

testParseLongTrue :: Test
testParseLongTrue = TestLabel "Long true" . TestCase $
    parse' "#true" >>= (@?= Boolean True)

testParseLongFalse :: Test
testParseLongFalse = TestLabel "Long false" . TestCase $
    parse' "#false" >>= (@?= Boolean False)

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
    parse' "identifier" >>= (@?= Symbol "identifier")

testParsePipedIdentifier :: Test
testParsePipedIdentifier = TestLabel "Piped identifier" . TestCase $
    parse' "|an IDENTIFIER \\x0;|" >>= (@?= Symbol "an IDENTIFIER \NUL")

identifierTests :: Test
identifierTests = TestLabel "Identifiers" $ TestList [
    testParseIdentifier,
    testParsePipedIdentifier
    ]

-- Numbers

testParseInteger :: Test
testParseInteger = TestLabel "Parse integer" . TestCase $
    parse' "10" >>= (@?= Number (Real (Rational (Integer 10))))

testParseDouble :: Test
testParseDouble = TestLabel "Parse double" . TestCase $
    parse' "12.34" >>= (@?= Number (Real (Rational (Double 12.34))))

testParseRatio :: Test
testParseRatio = TestLabel "Parse ratio" . TestCase $
    parse' "1/2" >>= (@?= Number (Real (Rational (Ratio 1 2))))

testParseComplex :: Test
testParseComplex = TestLabel "Parse complex" . TestCase $
    parse' "1+2i" >>= (@?= Number (Rectangular (Rational (Integer 1)) (Rational (Integer 2))))

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
    parse' "#\\alarm" >>= (@?= Character '\7'),
    parse' "#\\backspace" >>= (@?= Character '\8'),
    parse' "#\\delete" >>= (@?= Character '\127'),
    parse' "#\\escape" >>= (@?= Character '\27'),
    parse' "#\\newline" >>= (@?= Character '\n'),
    parse' "#\\null" >>= (@?= Character '\0'),
    parse' "#\\return" >>= (@?= Character '\r'),
    parse' "#\\space" >>= (@?= Character ' '),
    parse' "#\\tab" >>= (@?= Character '\t')
    ]

testHexCharacters :: Test
testHexCharacters = TestLabel "Hex characters" . TestList $ map TestCase [
    parse' "#\\x03BB" >>= (@?= Character 'λ'),
    parse' "#\\x03bb" >>= (@?= Character 'λ')
    ]

testCharacterLiterals :: Test
testCharacterLiterals = TestLabel "Character literals" . TestList $ map TestCase [
    parse' "#\\x" >>= (@?= Character 'x'),
    parse' "#\\1" >>= (@?= Character '1')
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
    parse' "\"\"" >>= (@?= String "")

testEscapedStringCharacters :: Test
testEscapedStringCharacters = TestLabel "Escaped string characters" . TestList $ map TestCase [
    parse' "\"\\a\"" >>= (@?= String "\7"),
    parse' "\"\\b\"" >>= (@?= String "\8"),
    parse' "\"\\t\"" >>= (@?= String "\t"),
    parse' "\"\\n\"" >>= (@?= String "\n"),
    parse' "\"\\r\"" >>= (@?= String "\r"),
    parse' "\"\\\"\"" >>= (@?= String "\""),
    parse' "\"\\\\\"" >>= (@?= String "\\"),
    parse' "\"\\|\"" >>= (@?= String "|"),
    parse' "\"\\x0;\"" >>= (@?= String "\NUL")
    ]

testMultiCharacterString ::Test
testMultiCharacterString = TestLabel "Multi-character string" . TestCase $
    parse' "\"abc123 \\|\"" >>= (@?= String "abc123 |")

stringTests :: Test
stringTests = TestLabel "Strings" $ TestList [
    testEmptyString,
    testEscapedStringCharacters,
    testMultiCharacterString]

-- Vectors

testEmptyVector :: Test
testEmptyVector = TestLabel "Empty vector" . TestCase $
    parse' "#()" >>= (@?= Vector [])

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
