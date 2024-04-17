module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import Test.HUnit
import Control.Exception (SomeException)

import Crafty.Parse
import GHC.Stack (HasCallStack)

-- Utilities

testFileName :: String
testFileName = "test.scm"

parse :: String -> Either SomeException [Datum]
parse = Crafty.Parse.read testFileName

assertRight :: HasCallStack => Show a => Either a b -> IO b
assertRight e = case e of
    Left error' -> assertFailure $ show error'
    Right value -> return value

parse' :: HasCallStack => String -> IO [Datum]
parse' = assertRight . parse

parsesTo :: HasCallStack => String -> [Datum] -> Assertion
parsesTo source datum' = parse' source >>= (@?= datum')

-- TODO: Use this
testCase :: String -> Assertion -> Test
testCase label = TestLabel label . TestCase

testCases :: String -> [Test] -> Test
testCases label = TestLabel label . TestList

-- Tests

-- TODO: Test failure cases

-- Booleans

testShortTrue :: Test
testShortTrue = testCase "Short true" $
    "#t" `parsesTo` [Boolean True]

testShortFalse :: Test
testShortFalse = testCase "Short false" $
    "#f" `parsesTo` [Boolean False]

testLongTrue :: Test
testLongTrue = testCase "Long true" $
    "#true" `parsesTo` [Boolean True]

testLongFalse :: Test
testLongFalse = testCase "Long false" $
    "#false" `parsesTo` [Boolean False]

booleanTests :: Test
booleanTests = testCases "Booleans" [
    testShortTrue,
    testShortFalse,
    testLongTrue,
    testLongFalse
    ]

-- Identifiers

testIdentifier :: Test
testIdentifier = testCase "Identifier" $
    "identifier" `parsesTo` [Symbol "identifier"]

testPipedIdentifier :: Test
testPipedIdentifier = testCase "Piped identifier" $
    "|an IDENTIFIER \\x0;|" `parsesTo` [Symbol "an IDENTIFIER \NUL"]

identifierTests :: Test
identifierTests = testCases "Identifiers" [
    testIdentifier,
    testPipedIdentifier
    ]

-- Numbers

testInteger :: Test
testInteger = testCase "Integer" $
    "10" `parsesTo` [Number . Real . Rational $ Integer 10]

testNegativeInteger :: Test
testNegativeInteger = testCase "Negative integer" $
    "-10" `parsesTo` [Number . Real . Rational $ Integer (-10)]

testDouble :: Test
testDouble = testCase "Double" $
    "12.34" `parsesTo` [Number . Real . Rational $ Double 12.34]

testNegativeDouble :: Test
testNegativeDouble = testCase "Negative double" $
    "-12.34" `parsesTo` [Number . Real . Rational $ Double (-12.34)]

testDoubleWithTrailingZeros :: Test
testDoubleWithTrailingZeros = testCase "Double with trailing zeros" $
    "12.30400" `parsesTo` [Number . Real . Rational $ Double 12.304]

testExactDecimal :: Test
testExactDecimal = testCase "Exact decimal" $
    "#e12.340" `parsesTo` [Number . Real . Rational $ Ratio 617 50]

testNormalizeExactDecimalWithExponent :: Test
testNormalizeExactDecimalWithExponent = testCase "Normalize exact decimal" $
    "#e1.23400e1" `parsesTo` [Number . Real . Rational $ Ratio 617 50]

testTrailingDecimal :: Test
testTrailingDecimal = testCase "Trailing decimal" $
    ".123400" `parsesTo` [Number . Real . Rational $ Double 0.1234]

testExactTrailingDecimal :: Test
testExactTrailingDecimal = testCase "Exact trailing decimal" $
    "#e.12340" `parsesTo` [Number . Real . Rational $ Ratio 617 5000]

testRatio :: Test
testRatio = testCase "Parse ratio" $
    "1/2" `parsesTo` [Number . Real . Rational $ Ratio 1 2]

testNegativeRatio :: Test
testNegativeRatio = testCase "Negative ratio" $
    "-1/2" `parsesTo` [Number . Real . Rational $ Ratio (-1) 2]

testNormalizeRatio :: Test
testNormalizeRatio = testCase "Normalize ratio" $
    "2/4" `parsesTo` [Number . Real . Rational $ Ratio 1 2]

testNormalizeNegativeRatio :: Test
testNormalizeNegativeRatio = testCase "Normalize negative ratio" $
    "-2/4" `parsesTo` [Number . Real . Rational $ Ratio (-1) 2]

testComplex :: Test
testComplex = testCase "Parse complex" $
    "1+2i" `parsesTo` [Number (Rectangular (Rational $ Integer 1) (Rational $ Integer 2))]

testComplexWithNegativeRealPart :: Test
testComplexWithNegativeRealPart = testCase "Complex with negative real part" $
    "-1+2i" `parsesTo` [Number (Rectangular (Rational $ Integer (-1)) (Rational $ Integer 2))]

testComplexWithNegativeImaginaryPart :: Test
testComplexWithNegativeImaginaryPart = testCase "Complex with negative imaginary part" $
    "1-2i" `parsesTo` [Number (Rectangular (Rational $ Integer 1) (Rational $ Integer (-2)))]

testComplexWithOnlyRealPart :: Test
testComplexWithOnlyRealPart = testCase "Complex with only real part" $
    "5+0i" `parsesTo` [Number $ Rectangular (Rational $ Integer 5) (Rational $ Integer 0)]

testComplexWithOnlyImaginaryPart :: Test
testComplexWithOnlyImaginaryPart = testCase "Complex with only imaginary part" $
    "+10i" `parsesTo` [Number $ Rectangular (Rational $ Integer 0) (Rational $ Integer 10)]

testComplexWithOnlyNegativeImaginaryPart :: Test
testComplexWithOnlyNegativeImaginaryPart = testCase "Complex with only negative imaginary part" $
    "-10i" `parsesTo` [Number $ Rectangular (Rational $ Integer 0) (Rational $ Integer (-10))]

testRealPlusI :: Test
testRealPlusI = testCase "Complex of real plus i" $
    "10+i" `parsesTo` [Number $ Rectangular (Rational $ Integer 10) (Rational $ Integer 1)]

testRealMinusI :: Test
testRealMinusI = testCase "Complex of real minus i" $
    "10-i" `parsesTo` [Number $ Rectangular (Rational $ Integer 10) (Rational $ Integer (-1))]

testPolarComplex :: Test
testPolarComplex = testCase "Polar complex" $
    "1@2" `parsesTo` [Number $ Polar (Rational $ Integer 1) (Rational $ Integer 2)]

numberTests :: Test
numberTests = testCases "Numbers" [
    testInteger,
    testNegativeInteger,
    testDouble,
    testNegativeDouble,
    testDoubleWithTrailingZeros,
    testExactDecimal,
    testNormalizeExactDecimalWithExponent,
    testTrailingDecimal,
    testExactTrailingDecimal,
    testRatio,
    testNegativeRatio,
    testNormalizeRatio,
    testNormalizeNegativeRatio,
    testComplex,
    testComplexWithNegativeRealPart,
    testComplexWithNegativeImaginaryPart,
    testComplexWithOnlyRealPart,
    testComplexWithOnlyImaginaryPart,
    testComplexWithOnlyNegativeImaginaryPart,
    testRealPlusI,
    testRealMinusI,
    testPolarComplex
    ]

-- Characters

testNamedCharacters :: Test
testNamedCharacters = testCases "Named characters" $ map TestCase [
    "#\\alarm"     `parsesTo` [Character '\7'],
    "#\\backspace" `parsesTo` [Character '\8'],
    "#\\delete"    `parsesTo` [Character '\127'],
    "#\\escape"    `parsesTo` [Character '\27'],
    "#\\newline"   `parsesTo` [Character '\n'],
    "#\\null"      `parsesTo` [Character '\0'],
    "#\\return"    `parsesTo` [Character '\r'],
    "#\\space"     `parsesTo` [Character ' '],
    "#\\tab"       `parsesTo` [Character '\t']
    ]

testHexCharacters :: Test
testHexCharacters = testCases "Hex characters" $ map TestCase [
    "#\\x03BB" `parsesTo` [Character 'λ'],
    "#\\x03bb" `parsesTo` [Character 'λ']
    ]

testCharacterLiterals :: Test
testCharacterLiterals = testCases "Character literals" $ map TestCase [
    "#\\x" `parsesTo` [Character 'x'],
    "#\\1" `parsesTo` [Character '1']
    ]

characterTests :: Test
characterTests = testCases "Numbers" [
    testNamedCharacters,
    testHexCharacters,
    testCharacterLiterals
    ]

-- Strings

testEmptyString :: Test
testEmptyString = testCase "Empty string" $
    "\"\"" `parsesTo` [String ""]

testEscapedStringCharacters :: Test
testEscapedStringCharacters = testCases "Escaped string characters" $ map TestCase [
    "\"\\a\""   `parsesTo` [String "\7"],
    "\"\\b\""   `parsesTo` [String "\8"],
    "\"\\t\""   `parsesTo` [String "\t"],
    "\"\\n\""   `parsesTo` [String "\n"],
    "\"\\r\""   `parsesTo` [String "\r"],
    "\"\\\"\""  `parsesTo` [String "\""],
    "\"\\\\\""  `parsesTo` [String "\\"],
    "\"\\|\""   `parsesTo` [String "|"],
    "\"\\x0;\"" `parsesTo` [String "\NUL"]
    ]

testMultiCharacterString ::Test
testMultiCharacterString = testCase "Multi-character string" $
    "\"abc123 \\|\"" `parsesTo` [String "abc123 |"]

stringTests :: Test
stringTests = testCases "Strings" [
    testEmptyString,
    testEscapedStringCharacters,
    testMultiCharacterString]

-- Vectors

testEmptyVector :: Test
testEmptyVector = testCase "Empty vector" $
    "#()" `parsesTo` [Vector []]

testHeterogeneousVector :: Test
testHeterogeneousVector = testCase "Heterogeneous vector" $
    "#(0 (2 2 2 2) \"Anna\")" `parsesTo` [Vector [
        zero,
        List [two, two, two, two],
        String "Anna"
    ]]
    where
        zero = Number . Real . Rational $ Integer 0
        two = Number . Real . Rational $ Integer 2

testSymbolVector :: Test
testSymbolVector = testCase "Character vector" $
    "#(a b c)" `parsesTo` [Vector [Symbol "a", Symbol "b", Symbol "c"]]

testIntegerVector :: Test
testIntegerVector = testCase "Integer vector" $
    "#(1 1 2 3 5 8 13 21)" `parsesTo` [Vector [
        integer 1,
        integer 1,
        integer 2,
        integer 3,
        integer 5,
        integer 8,
        integer 13,
        integer 21
    ]]
    where
        integer = Number . Real . Rational . Integer

vectorTests :: Test
vectorTests = testCases "Vectors" [
    testEmptyVector,
    testHeterogeneousVector,
    testSymbolVector,
    testIntegerVector
    ]

-- Bytevectors

testEmptyBytevector :: Test
testEmptyBytevector = testCase "Empty bytevector" $
    "#u8()" `parsesTo` [ByteVector []]

testBasicBytevector :: Test
testBasicBytevector = testCase "Basic bytevector" $
    "#u8(0 10 5)" `parsesTo` [ByteVector [0, 10, 5]]

bytevectorTests :: Test
bytevectorTests = testCases "Bytevectors" [
    testEmptyBytevector,
    testBasicBytevector
    ]

-- Whitespace and comments

testEmptySource :: Test
testEmptySource = testCase "Empty source" $
    "" `parsesTo` []

testOnlyWhitespace :: Test
testOnlyWhitespace = testCase "Only whitespace" $
    "\n\r \t" `parsesTo` []

testLineComment :: Test
testLineComment = testCase "Line comment" $
    "; Some text" `parsesTo` []

testLineCommentWithData :: Test
testLineCommentWithData = testCase "Line comment with data" $
    "x ; Comment 1\ny;Comment 2\nz" `parsesTo` [Symbol "x", Symbol "y", Symbol "z"]

testBlockComment :: Test
testBlockComment = testCase "Block comment" $
    "#| Comment |#" `parsesTo` []

testBlockCommentWithData :: Test
testBlockCommentWithData = testCase "Block comment with data" $
    "x #| Comment\n1 |# y #| Comment 2 |# z" `parsesTo` [Symbol "x", Symbol "y", Symbol "z"]

testNestedBlockComments :: Test
testNestedBlockComments = testCase "Nested block comments" $
    "#| Outer #| Inner |# Outer |#" `parsesTo` []

testDatumComment :: Test
testDatumComment = testCase "Datum comment" $
    "#; x" `parsesTo` []

testDatumCommentWithData :: Test
testDatumCommentWithData = testCase "Datum comment with data" $
    "x #;(a b c) y #; 1 z" `parsesTo` [Symbol "x", Symbol "y", Symbol "z"]

whitespaceTests :: Test
whitespaceTests = testCases "Whitespace" [
    testEmptySource,
    testOnlyWhitespace,
    testLineComment,
    testLineCommentWithData,
    testBlockComment,
    testBlockCommentWithData,
    testNestedBlockComments,
    testDatumComment,
    testDatumCommentWithData
    ]

-- Directives

testFoldCase :: Test
testFoldCase = testCase "Fold case" $
    "#!fold-case aBc XyZ ß" `parsesTo` [Symbol "abc", Symbol "xyz", Symbol "ss"]

testNoFoldCase :: Test
testNoFoldCase = testCase "No fold case" $
    "#!fold-case aBc #!no-fold-case XyZ ß" `parsesTo` [Symbol "abc", Symbol "XyZ", Symbol "ß"]

testResetToFoldCase :: Test
testResetToFoldCase = testCase "Reset to fold case" $
    "#!fold-case aBc #!no-fold-case DeF #!fold-case gHi" `parsesTo` [Symbol "abc", Symbol "DeF", Symbol "ghi"]

testResetToNoFoldCase :: Test
testResetToNoFoldCase = testCase "Reset to no fold case" $
    "#!no-fold-case aBc #!fold-case DeF #!no-fold-case gHi" `parsesTo` [Symbol "aBc", Symbol "def", Symbol "gHi"]

directiveTests :: Test
directiveTests = testCases "Directives" [
    testFoldCase,
    testNoFoldCase,
    testResetToFoldCase,
    testResetToNoFoldCase
    ]

-- All tests

allTests :: Test
allTests = TestList [
    identifierTests,
    numberTests,
    booleanTests,
    characterTests,
    stringTests,
    vectorTests,
    bytevectorTests,
    whitespaceTests,
    directiveTests
    ]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if (failures counts > 0 || errors counts > 0) then exitFailure else exitSuccess
