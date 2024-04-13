module Crafty.Parse where

import Control.Monad (void)
import Data.Char (readLitChar, digitToInt)
import Data.Functor (($>))
import Text.Parsec ((<|>), (<?>))
import qualified Text.Parsec as Parsec
import Prelude hiding (Rational, Real)
import Control.Exception (SomeException, Exception (toException))

-- read

data FoldCaseState = FoldCase | NoFoldCase deriving (Show)

type Parser = Parsec.Parsec String FoldCaseState

read :: String -> String -> Either SomeException [Datum]
read file source = case Parsec.runParser (data' <* Parsec.eof) FoldCase file source of
    Left e -> Left $ toException e
    Right result -> Right result

leftParenthesis :: Parser ()
leftParenthesis = void $ Parsec.char '('

rightParenthesis :: Parser ()
rightParenthesis = void $ Parsec.char ')'

boolean :: Parser Bool
boolean = true <|> false <?> "boolean"

true :: Parser Bool
true = (Parsec.string' "#true" <|> Parsec.string' "#t") $> True

false :: Parser Bool
false = (Parsec.string' "#false" <|> Parsec.string' "#f") $> False

vectorStart :: Parser ()
vectorStart = void $ Parsec.string' "#("

byteVectorStart :: Parser ()
byteVectorStart = void $ Parsec.string' "#u8("

quote :: Parser Char
quote = Parsec.char '\''

backtick :: Parser Char
backtick = Parsec.char '`'

comma :: Parser Char
comma = Parsec.char ','

dot :: Parser Char
dot = Parsec.char '.'

at :: Parser Char
at = Parsec.char '@'

commaAt :: Parser String
commaAt = do
    c <- comma
    a <- at
    return [c, a]

character :: Parser Char
character = Parsec.try namedCharacter <|> Parsec.try hexCharacter <|> escapedCharacter

namedCharacter :: Parser Char
namedCharacter = do
    void $ Parsec.string' "#\\"
    (Parsec.string' "alarm" $> '\7')
        <|> (Parsec.string' "backspace" $> '\8')
        <|> (Parsec.string' "delete" $> '\127')
        <|> (Parsec.string' "escape" $> '\27')
        <|> (Parsec.string' "newline" $> '\n')
        <|> (Parsec.string' "null" $> '\0')
        <|> (Parsec.string' "return" $> '\r')
        <|> (Parsec.string' "space" $> ' ')
        <|> (Parsec.string' "tab" $> '\t')

hexCharacter :: Parser Char
hexCharacter = do
    void $ Parsec.string' "#\\x"
    scalar <- Parsec.many1 Parsec.hexDigit
    maybe
        (fail $ "Unparseable hex scalar: " ++ scalar)
        return
        (readHexScalar scalar)

escapedCharacter :: Parser Char
escapedCharacter = Parsec.string' "#\\" *> Parsec.anyChar

string :: Parser String
string = do
    void $ Parsec.char '"'
    elements <- Parsec.many stringElement
    void $ Parsec.char '"'
    return $ concat elements

stringElement :: Parser String
stringElement = anyCharacter
    <|> Parsec.try escapedStringElement
    <|> Parsec.try mnemonicEscape
    <|> Parsec.try escapedNewline
    <|> Parsec.try inlineHexEscape
    <|> Parsec.try (pure <$> escapedPipe)
    <?> "string element"

anyCharacter :: Parser String
anyCharacter = return <$> Parsec.noneOf ['\\', '"', '|']

escapedStringElement :: Parser String
escapedStringElement = do
    void $ Parsec.char '\\'
    char <- Parsec.oneOf "\\\""
    return [char]

mnemonicEscape :: Parser String
mnemonicEscape = (Parsec.string' "\\a" $> "\a")
    <|> (Parsec.string' "\\b" $> "\b")
    <|> (Parsec.string' "\\t" $> "\t")
    <|> (Parsec.string' "\\n" $> "\n")
    <|> (Parsec.string' "\\r" $> "\r")

readHexScalar :: String -> Maybe Char
readHexScalar scalar = case readLitChar ("\\x" ++ scalar) of
    ((c, _):_) -> return c
    _ -> fail $ "Unparseable hex scalar: " ++ scalar

inlineHexEscape :: Parser String
inlineHexEscape = do
    void $ Parsec.string' "\\x"
    scalar <- Parsec.many1 Parsec.hexDigit
    void $ Parsec.char ';'
    pure $ maybe
        (fail $ "Unparseable hex scalar: " ++ scalar)
        return
        (readHexScalar scalar)

escapedNewline :: Parser String
escapedNewline = Parsec.char '\\'
    *> Parsec.many intralineWhitespace
    *> lineEnding
    <* Parsec.many intralineWhitespace

intralineWhitespace :: Parser Char
intralineWhitespace = Parsec.oneOf " \t"

lineEnding :: Parser String
lineEnding = pure <$> Parsec.char '\n'
    <|> Parsec.string' "\r\n"
    <|> pure <$> Parsec.char '\r'
    <?> "line ending"

-- TODO: Integrate with foldCase (CI)
identifier :: Parser String
identifier = Parsec.try initialSubsequents
    <|> Parsec.try pipedIdentifier
    <|> peculiarIdentifier
    <?> "identifier"

initialSubsequents :: Parser String
initialSubsequents = do
    initial' <- initial
    subsequents <- Parsec.many subsequent
    return (initial':subsequents)

pipedIdentifier :: Parser String
pipedIdentifier = do
    void verticalLine
    elements <- Parsec.many symbolElement
    void verticalLine
    return $ concat elements

initial :: Parser Char
initial = letter <|> specialInitial

letter :: Parser Char
letter = Parsec.letter

specialInitial :: Parser Char
specialInitial = Parsec.oneOf "!$%&*/:<=>?^_~"

subsequent :: Parser Char
-- subsequent = (pure <$> initial) <|> (pure <$> digit) <|> (pure <$> specialSubsequent)
subsequent = initial <|> digit <|> specialSubsequent

digit :: Parser Char
digit = Parsec.digit

explicitSign :: Parser Char
explicitSign = Parsec.oneOf "+-"

specialSubsequent :: Parser Char
specialSubsequent = explicitSign <|> dot <|> at

verticalLine :: Parser Char
verticalLine = Parsec.char '|'

symbolElement :: Parser String
symbolElement = Parsec.try (pure <$> Parsec.noneOf "|\\")
    <|> Parsec.try inlineHexEscape
    <|> Parsec.try mnemonicEscape
    <|> Parsec.try (pure <$> escapedPipe)

-- TODO: Factor out an "escaped" parser
escapedPipe :: Parser Char
escapedPipe = Parsec.char '\\' *> Parsec.char '|'

peculiarIdentifier :: Parser String
peculiarIdentifier = Parsec.try (do
        sign <- explicitSign
        signSubsequent' <- signSubsequent
        subsequents <- Parsec.many subsequent
        return $ [sign, signSubsequent'] ++ subsequents)
    <|> Parsec.try (do
        sign <- explicitSign
        dot' <- dot
        dotSubsequent' <- dotSubsequent
        subsequents <- Parsec.many subsequent
        return $ [sign, dot', dotSubsequent'] ++ subsequents)
    <|> Parsec.try (do
        dot' <- dot
        dotSubsequent' <- dotSubsequent
        subsequents <- Parsec.many subsequent
        return $ [dot', dotSubsequent'] ++ subsequents)
    -- FIXME: This is last because otherwise +, i will match before +i
    -- (This will be fixed when whitespace is required between tokens)
    <|> Parsec.try (pure <$> explicitSign)

signSubsequent :: Parser Char
signSubsequent = initial <|> explicitSign <|> at

dotSubsequent :: Parser Char
dotSubsequent = signSubsequent <|> dot

-- Number (TODO)
-- TODO: Exactness

-- TODO: Encode exactness
-- data Number 
--     = Integer Integer
--     | Double Double
--     | Fraction Integer Integer
--     | InfNumber Inf
--     | NanNumber Nan
--     deriving (Show)

-- TODO: Clean this up
-- multiply :: Number -> Int -> Number
-- multiply n i = case n of
--     Integer x -> Integer (x * i)
--     Double x -> Double (x * i)
--     Fraction x y -> Fraction (x * i) y
--     _ -> n -- FIXME

-- Number

number :: Parser Complex
number = Parsec.try (number' Binary)
    <|> Parsec.try (number' Octal)
    <|> Parsec.try (number' Decimal)
    <|> Parsec.try (number' Hexadecimal)
    <?> "number"

-- TODO: Figure out what to do with exactness/prefix/etc.
number' :: Radix -> Parser Complex
number' r = do
    prefix' <- prefix r
    n <- complex r
    return n

-- Complex

data Complex = Rectangular Real Real | Polar Real Real | Real Real deriving (Show, Eq)

iPlus :: Parser Complex
iPlus = Parsec.string' "+i" $> Rectangular (Rational $ Integer 0) (Rational . Integer $ 1)

iMinus :: Parser Complex
iMinus = Parsec.string' "-i" $> Rectangular (Rational $ Integer 0) (Rational . Integer $ -1)

infI :: Parser Complex
infI = do
    inf' <- inf
    void $ Parsec.char 'i'
    return $ Rectangular (Rational $ Integer 0) inf'

nanI :: Parser Complex
nanI = do
    nan' <- nan
    void $ Parsec.char 'i'
    return $ Rectangular (Rational $ Integer 0) nan'

infNanI :: Parser Complex
infNanI = infI <|> nanI

positiveUrealI :: Radix -> Parser Complex
positiveUrealI r = do
    void $ Parsec.char '+'
    ur <- ureal r
    void $ Parsec.char 'i'
    return $ Rectangular (Rational $ Integer 0) ur

negativeUrealI :: Radix -> Parser Complex
negativeUrealI r = do
    void $ Parsec.char '-'
    ur <- ureal r
    void $ Parsec.char 'i'
    -- TODO: Make negative
    -- return $ Inexact (Integer 0) (multiply ur -1)
    return $ Rectangular (Rational $ Integer 0) ur

realInfNanI :: Radix -> Parser Complex
realInfNanI r = do
    real' <- real r
    infNan' <- infNan
    void $ Parsec.char 'i'
    return $ Rectangular real' infNan'

realNegativeI :: Radix -> Parser Complex
realNegativeI r = do
    real' <- real r
    void $ Parsec.string "-i"
    -- TODO: Negative
    return $ Rectangular real' (Rational $ Integer 0)

realPositiveI :: Radix -> Parser Complex
realPositiveI r = do
    real' <- real r
    void $ Parsec.string "+i"
    return $ Rectangular real' (Rational $ Integer 0)

realMinusUrealI :: Radix -> Parser Complex
realMinusUrealI r = do
    real' <- real r
    void $ Parsec.char '-'
    imaginary <- ureal r
    void $ Parsec.char 'i'
    -- TODO: Negative
    return $ Rectangular real' imaginary

realPlusUrealI :: Radix -> Parser Complex
realPlusUrealI r = do
    real' <- real r
    void $ Parsec.char '+'
    imaginary <- ureal r
    void $ Parsec.char 'i'
    return $ Rectangular real' imaginary

polarComplex :: Radix -> Parser Complex
polarComplex r = do
    r' <- real r
    void at
    theta <- real r
    return $ Polar r' theta

realComplex :: Radix -> Parser Complex
realComplex r = Real <$> real r

complex :: Radix -> Parser Complex
complex r = iPlus
    <|> Parsec.try iMinus
    <|> Parsec.try infNanI
    <|> Parsec.try (polarComplex r)
    <|> Parsec.try (positiveUrealI r)
    <|> Parsec.try (negativeUrealI r)
    <|> Parsec.try (realInfNanI r)
    <|> Parsec.try (realNegativeI r)
    <|> Parsec.try (realPositiveI r)
    <|> Parsec.try (realMinusUrealI r)
    <|> Parsec.try (realPlusUrealI r)
    <|> Parsec.try (realComplex r)

-- Real

data Real = Nan | PositiveInf | NegativeInf | Rational Rational deriving (Show, Eq)

real :: Radix -> Parser Real
real r = Parsec.try infNan <|> Parsec.try signedReal
    where
        signedReal = do
            sign' <- Parsec.option Positive sign
            n <- ureal r
            return n
            -- TODO: Sign
            -- return . RealNumber $ case n of
            --     Integer i -> Integer s * fromIntegral i
            --     Double d -> Double (fromIntegral s) * fromIntegral d
            --     Fraction x y -> Fraction (fromIntegral s * x) y

ureal :: Radix -> Parser Real
ureal r = Rational <$> case r of
    Decimal -> Parsec.try (ratio r) <|> Parsec.try decimal10 <|> Parsec.try (Integer <$> uinteger r)
    _ -> Parsec.try (ratio r) <|> Parsec.try (Integer <$> uinteger r)

-- Rational

data Rational = Ratio Integer Integer | Double Double | Integer Integer deriving (Show, Eq)

ratio :: Radix -> Parser Rational
ratio r = do
    x <- uinteger r
    void $ Parsec.char '/'
    y <- uinteger r
    return $ Ratio x y

decimal10 :: Parser Rational
decimal10 = Parsec.try fractionalDecimal10
    <|> Parsec.try fullDecimal10
    <|> Parsec.try uintegerDecimal10

uintegerDecimal10 :: Parser Rational
uintegerDecimal10 = do
    u <- uinteger Decimal
    e <- Parsec.option 0 suffix
    return $ Integer (u * 10 ^ e)

-- TODO: Reuse
parseDigits :: Radix -> [Char] -> Integer
parseDigits r = toInteger . foldl (\x d -> x * r' + digitToInt d) 0
    where
        r' = case r of
            Binary -> 2
            Octal -> 8
            Decimal -> 10
            Hexadecimal -> 16

readMantissa :: String -> Double
readMantissa s = foldl (\x (i, d) -> x + fromIntegral (digitToInt d) * 10 ^^ (-i)) 0 (zip [1..] s)

fractionalDecimal10 :: Parser Rational
fractionalDecimal10 = do
    void dot
    d <- readMantissa <$> Parsec.many1 (digit' Decimal)
    s <- Parsec.option 0 suffix
    return $ Double (d * 10 ^ s)

fullDecimal10 :: Parser Rational
fullDecimal10 = do
    integerPart <- uinteger Decimal
    void dot
    fractionalPart <- readMantissa <$> Parsec.many (digit' Decimal)
    s <- Parsec.option 0 suffix
    return . Double $ (fromIntegral integerPart + fractionalPart) * 10 ^ s

uinteger :: Radix -> Parser Integer
-- TODO: Remove duplication with suffix
uinteger r = parseDigits r <$> Parsec.many1 (digit' r)

prefix :: Radix -> Parser Exactness
prefix r = Parsec.try (radix r *> exactness') <|> Parsec.try (exactness' <* radix r)
    where
        exactness' = Parsec.option Exact exactness

inf :: Parser Real
inf = Parsec.string' "+inf.0" $> PositiveInf
    <|> Parsec.string' "-inf.0" $> NegativeInf

nan :: Parser Real
nan = (Parsec.string' "+nan.0" <|> Parsec.string' "-nan.0") $> Nan

infNan :: Parser Real
infNan = inf <|> nan

-- TODO: Preserve suffix for exactness and only multiply it in at runtime?
suffix :: Parser Integer
suffix = do
    exponentMarker
    sign' <- Parsec.option Positive sign
    exponent' <- Parsec.many1 $ digit' Decimal
    let n = parseDigits Decimal exponent'
    return $ case sign' of
        Positive -> n
        Negative -> -n

exponentMarker :: Parser ()
exponentMarker = void $ Parsec.char 'e'

data Sign = Positive | Negative deriving (Show)

sign :: Parser Sign
sign = positive <|> negative
    where
        positive = Parsec.char '+' $> Positive
        negative = Parsec.char '-' $> Negative

data Exactness = Exact | Inexact deriving (Show)

exactness :: Parser Exactness
exactness = Parsec.try exact <|> Parsec.try inexact
    where
        exact = Parsec.string' "#e" $> Exact
        inexact = Parsec.string' "#i" $> Inexact

data Radix = Binary | Octal | Decimal | Hexadecimal deriving (Show)

radix :: Radix -> Parser ()
radix r = case r of
    Binary -> void $ Parsec.string' "#b"
    Octal -> void $ Parsec.string' "#o"
    Decimal -> Parsec.optional (Parsec.try $ Parsec.string' "#d")
    Hexadecimal -> void $ Parsec.string' "#x"

digit' :: Radix -> Parser Char
digit' r = case r of
    Binary -> Parsec.char '0' <|> Parsec.char '1'
    Octal -> Parsec.octDigit
    Decimal -> digit
    Hexadecimal -> Parsec.hexDigit

-- Whitespace

whitespace :: Parser ()
whitespace = void intralineWhitespace <|> void lineEnding <?> "whitespace"

comment :: Parser ()
comment = lineComment <|> nestedComment <?> "comment" -- <|> datumComment

lineComment :: Parser ()
lineComment = void $ Parsec.char ';' *> Parsec.manyTill Parsec.anyChar (Parsec.try lineEnding)

nestedComment :: Parser ()
nestedComment = void $ Parsec.string' "#|" *> commentText *> Parsec.many commentCont *> Parsec.string' "|#"

commentText :: Parser ()
commentText = void $ Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string' "#|" <|> Parsec.string' "|#")

commentCont :: Parser ()
commentCont = nestedComment <|> commentText

-- TODO: Do this after datum parser
-- datumComment :: Parser ()
-- datumComment = Parsec.string' "#;" *> intertokenSpace *> datum

intertokenSpace :: Parser ()
intertokenSpace = void $ Parsec.many1 atmosphere

atmosphere :: Parser ()
atmosphere = whitespace <|> comment <|> directive

directive :: Parser ()
directive = foldCaseDirective <|> noFoldCaseDirective <?> "directive"

foldCaseDirective :: Parser ()
foldCaseDirective = Parsec.string' "#!fold-case" *> Parsec.putState FoldCase

noFoldCaseDirective :: Parser ()
noFoldCaseDirective = Parsec.string' "#!no-fold-case" *> Parsec.putState NoFoldCase

-- Parsing
-- Corresponds to Section 7.1.2 (External Representations) in the R7RS spec

data Datum
    = Boolean Bool
    | Number Complex
    | Character Char
    | String String
    | Symbol String
    -- | ByteVector [Datum] -- TODO
    | List [Datum]
    | Vector [Datum]
    | Labeled Integer Datum
    | Label Integer
    deriving (Show, Eq)

datum :: Parser Datum
datum = Parsec.try simpleDatum
    <|> Parsec.try compoundDatum
    <|> Parsec.try labeled
    <|> Label <$> Parsec.try label <* Parsec.char '#'

data' :: Parser [Datum]
data' = Parsec.sepBy datum intertokenSpace

data1 :: Parser [Datum]
data1 = Parsec.sepBy1 datum intertokenSpace

label :: Parser Integer
label = Parsec.char '#' *> uinteger Decimal

labeled :: Parser Datum
labeled = do
    label' <- label
    void $ Parsec.char '='
    Labeled label' <$> datum

vector :: Parser [Datum]
vector = vectorStart *> data' <* rightParenthesis

-- TODO: Abbreviation/abbrev prefix

list :: Parser [Datum]
list = Parsec.try properList <|> Parsec.try improperList
    where
        properList = leftParenthesis *> data' <* rightParenthesis
        improperList = do
            void leftParenthesis
            initial' <- data1
            void dot
            tail' <- datum
            void rightParenthesis
            return $ initial' ++ [tail']

-- TODO: Abbrev
compoundDatum :: Parser Datum
compoundDatum = List <$> Parsec.try list <|> Vector <$> Parsec.try vector

symbol :: Parser String
symbol = identifier

simpleDatum :: Parser Datum
simpleDatum = Boolean <$> Parsec.try boolean
    <|> Number <$> Parsec.try number
    <|> Character <$> Parsec.try character
    <|> String <$> Parsec.try string
    <|> Symbol <$> Parsec.try symbol
    -- <|> ByteVector <$> Parsec.try byteVector
