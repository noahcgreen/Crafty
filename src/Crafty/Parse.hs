module Crafty.Parse (
    read,
    readAll
    ) where

import Control.Monad (void)
import Data.Bits (toIntegralSized)
import Data.Complex hiding (Complex, polar)
import qualified Unicode.Char.Case as Case
import Data.Char (readLitChar, digitToInt)
import Data.Functor (($>))
import Data.Word (Word8)
import Text.Parsec ((<|>), (<?>))
import qualified Text.Parsec as Parsec
import Prelude hiding (read, Rational, Real)
import Control.Exception (SomeException, Exception (toException))
import GHC.Real hiding (Real)

import Crafty.Datum

-- Numeric utilities

parseDigits :: Radix -> [Char] -> Integer
parseDigits r = toInteger . foldl (\x d -> x * r' + digitToInt d) 0
    where
        r' = case r of
            Binary -> 2
            Octal -> 8
            Decimal -> 10
            Hexadecimal -> 16

-- Misc types

data Radix = Binary | Octal | Decimal | Hexadecimal deriving (Show)

data Exactness = Exact | Inexact deriving (Show)

data Sign = Positive | Negative deriving (Show)

-- read

data FoldCaseState = FoldCase | NoFoldCase deriving (Show)

type Parser = Parsec.Parsec String FoldCaseState

wrapParsecError :: Either Parsec.ParseError a -> Either SomeException a
wrapParsecError x = case x of
    Left e -> Left $ toException e
    Right r -> Right r

read :: String -> String -> Either SomeException (Maybe Datum, String)
read file source = wrapParsecError $ Parsec.runParser read' NoFoldCase file source
    where
        read' = intertokenSpace *> do
            d <- Just <$> datum <|> Parsec.eof $> Nothing
            rest <- Parsec.getInput
            return (d, rest)

readAll :: String -> String -> Either SomeException [Datum]
readAll file source = do
    (md, rest) <- read file source
    case md of
        Nothing -> return []
        Just d -> do
            d' <- readAll file rest
            return $ d:d'

-- Trivial tokens

leftParenthesis :: Parser ()
leftParenthesis = void $ Parsec.char '('

rightParenthesis :: Parser ()
rightParenthesis = void $ Parsec.char ')'

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

-- Boolean

boolean :: Parser Bool
boolean = true <|> false <?> "boolean"

true :: Parser Bool
true = (Parsec.string' "#true" <|> Parsec.string' "#t") $> True

false :: Parser Bool
false = (Parsec.string' "#false" <|> Parsec.string' "#f") $> False

-- Vector

vectorStart :: Parser ()
vectorStart = void $ Parsec.string' "#("

vector :: Parser [Datum]
vector = vectorStart *> data' <* rightParenthesis

-- Bytevector

byteVectorStart :: Parser ()
byteVectorStart = void $ Parsec.string' "#u8("

-- uinteger of any radix
uinteger' :: Parser Integer
uinteger' = Parsec.try (uinteger Binary)
    <|> Parsec.try (uinteger Octal)
    <|> Parsec.try (uinteger Decimal)
    <|> Parsec.try (uinteger Hexadecimal)

byte :: Parser Word8
byte = do
    x <- number
    case asInteger x >>= toIntegralSized of
        Just b -> return b
        Nothing -> fail $ "Bytevector element is not a byte: " ++ show x

bytevector :: Parser [Word8]
bytevector = do
    byteVectorStart
    bytes <- many byte
    rightParenthesis
    return bytes

-- Character

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

-- String

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

-- Whitespace

intralineWhitespace :: Parser Char
intralineWhitespace = Parsec.oneOf " \t"

lineEnding :: Parser String
lineEnding = pure <$> Parsec.char '\n'
    <|> Parsec.string' "\r\n"
    <|> pure <$> Parsec.char '\r'
    <?> "line ending"

whitespace :: Parser ()
whitespace = void intralineWhitespace <|> void lineEnding <?> "whitespace"

comment :: Parser ()
comment = Parsec.try lineComment
    <|> Parsec.try nestedComment
    <|> Parsec.try datumComment
    <?> "comment"

lineComment :: Parser ()
lineComment = void $ do
    void $ Parsec.char ';'
    Parsec.manyTill Parsec.anyChar (Parsec.try $ void lineEnding <|> Parsec.eof)

blockCommentStart :: Parser ()
blockCommentStart = void $ Parsec.string "#|"

blockCommentEnd :: Parser ()
blockCommentEnd = void $ Parsec.string "|#"

nestedComment :: Parser ()
nestedComment = void $ do
    blockCommentStart
    commentText
    void $ Parsec.many commentCont
    blockCommentEnd

commentText :: Parser ()
commentText = void $ Parsec.notFollowedBy (blockCommentStart <|> blockCommentEnd) *> Parsec.anyChar

commentCont :: Parser ()
commentCont = Parsec.try nestedComment <|> Parsec.try commentText

datumComment :: Parser ()
datumComment = void $ Parsec.string "#;" *> intertokenSpace *> datum

intertokenSpace :: Parser ()
intertokenSpace = void $ Parsec.many atmosphere

atmosphere :: Parser ()
atmosphere = Parsec.try whitespace
    <|> Parsec.try comment
    <|> Parsec.try directive

-- Identifier

identifier :: Parser String
identifier = do
    name <- Parsec.try initialSubsequents
        <|> Parsec.try pipedIdentifier
        <|> peculiarIdentifier
        <?> "identifier"
    foldCaseState <- Parsec.getState
    return $ case foldCaseState of
        FoldCase -> name >>= Case.toCaseFoldString
        NoFoldCase -> name

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

escapedPipe :: Parser Char
escapedPipe = Parsec.char '\\' *> Parsec.char '|'

peculiarIdentifier :: Parser String
peculiarIdentifier = Parsec.try (do
        sign' <- explicitSign
        signSubsequent' <- signSubsequent
        subsequents <- Parsec.many subsequent
        return $ [sign', signSubsequent'] ++ subsequents)
    <|> Parsec.try (do
        sign' <- explicitSign
        dot' <- dot
        dotSubsequent' <- dotSubsequent
        subsequents <- Parsec.many subsequent
        return $ [sign', dot', dotSubsequent'] ++ subsequents)
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

-- Number

number :: Parser Number
number = Parsec.try (number' Binary)
    <|> Parsec.try (number' Octal)
    <|> Parsec.try (number' Decimal)
    <|> Parsec.try (number' Hexadecimal)
    <?> "number"

number' :: Radix -> Parser Number
number' r = do
    prefix' <- prefix r
    complex prefix' r

-- Complex

iPlus :: Parser Number
iPlus = Parsec.string' "+i" $> Complex (0 :+ 1)

iMinus :: Parser Number
iMinus = Parsec.string' "-i" $> Complex (0 :+ (-1))

infI :: Parser Number
infI = do
    inf' <- inf
    void $ Parsec.char 'i'
    return . Complex $ 0 :+ (Double 1/0)

nanI :: Parser Number
nanI = do
    nan' <- nan
    void $ Parsec.char 'i'
    return . Complex $ 0 :+ (Double 0/0)

infNanI :: Parser Number
infNanI = infI <|> nanI

positiveUrealI :: Maybe Exactness -> Radix -> Parser Number
positiveUrealI exactness' r = do
    void $ Parsec.char '+'
    ur <- ureal exactness' r
    void $ Parsec.char 'i'
    return . Complex $ 0 :+ ur

negativeUrealI :: Maybe Exactness -> Radix -> Parser Number
negativeUrealI exactness' r = do
    void $ Parsec.char '-'
    ur <- ureal exactness' r
    void $ Parsec.char 'i'
    return . Complex $ 0 :+ (-ur)

realInfNanI :: Maybe Exactness -> Radix -> Parser Number
realInfNanI exactness' r = do
    real' <- real exactness' r
    infNan' <- infNan
    void $ Parsec.char 'i'
    return . Complex $ real' :+ infNan'

realNegativeI :: Maybe Exactness -> Radix -> Parser Number
realNegativeI exactness' r = do
    real' <- real exactness' r
    void $ Parsec.string "-i"
    return . Complex $ real' :+ (-1)

realPositiveI :: Maybe Exactness -> Radix -> Parser Number
realPositiveI exactness' r = do
    real' <- real exactness' r
    void $ Parsec.string "+i"
    return . Complex $ real' :+ 1

realMinusUrealI :: Maybe Exactness -> Radix -> Parser Number
realMinusUrealI exactness' r = do
    real' <- real exactness' r
    void $ Parsec.char '-'
    imaginary <- ureal exactness' r
    void $ Parsec.char 'i'
    return $ Complex (real' :+ (-imaginary))

realPlusUrealI :: Maybe Exactness -> Radix -> Parser Number
realPlusUrealI exactness' r = do
    real' <- real exactness' r
    void $ Parsec.char '+'
    imaginary <- ureal exactness' r
    void $ Parsec.char 'i'
    return $ Complex (real' :+ imaginary)

polarComplex :: Maybe Exactness -> Radix -> Parser Number
polarComplex exactness' r = do
    r' <- real exactness' r
    void at
    theta <- real exactness' r
    return $ polar r' theta

realComplex :: Maybe Exactness -> Radix -> Parser Number
realComplex exactness' r = Real <$> real exactness' r

complex :: Maybe Exactness -> Radix -> Parser Number
complex exactness' r = iPlus
    <|> Parsec.try iMinus
    <|> Parsec.try infNanI
    <|> Parsec.try (polarComplex exactness' r)
    <|> Parsec.try (positiveUrealI exactness' r)
    <|> Parsec.try (negativeUrealI exactness' r)
    <|> Parsec.try (realInfNanI exactness' r)
    <|> Parsec.try (realNegativeI exactness' r)
    <|> Parsec.try (realPositiveI exactness' r)
    <|> Parsec.try (realMinusUrealI exactness' r)
    <|> Parsec.try (realPlusUrealI exactness' r)
    <|> Parsec.try (realComplex exactness' r)

-- Real

real :: Maybe Exactness -> Radix -> Parser Real
real exactness' r = Parsec.try infNan <|> Parsec.try signedReal
    where
        signedReal = do
            sign' <- Parsec.option Positive sign
            n <- ureal exactness' r
            return $ case sign' of
                Positive -> n
                Negative -> -n

ureal :: Maybe Exactness -> Radix -> Parser Real
ureal exactness' r = case r of
    Decimal -> Parsec.try (ratio r)
        <|> Parsec.try (decimal10 exactness')
        <|> Parsec.try (uinteger r >>= \x -> return . Rational $ x % 1)
    _ -> Parsec.try (ratio r)
        <|> Parsec.try (uinteger r >>= \x -> return . Rational $ x % 1)

-- Rational

ratio :: Radix -> Parser Real
ratio r = do
    x <- uinteger r
    void $ Parsec.char '/'
    y <- uinteger r
    return . Rational $ x % y

decimal10 :: Maybe Exactness -> Parser Real
decimal10 exactness' = Parsec.try (fractionalDecimal10 exactness')
    <|> Parsec.try (fullDecimal10 exactness')
    <|> Parsec.try uintegerDecimal10

uintegerDecimal10 :: Parser Real
uintegerDecimal10 = do
    u <- uinteger Decimal
    e <- Parsec.option 0 suffix
    return . Rational $ (u * 10 ^ e) % 1

fractionalDecimal10 :: Maybe Exactness -> Parser Real
fractionalDecimal10 exactness' = do
    void dot
    decimalDigits <- Parsec.many1 (digit' Decimal)
    s <- Parsec.option 0 suffix

    let p = s - toInteger (length decimalDigits)
    let allDigits = parseDigits Decimal decimalDigits

    return $ case exactness' of
        Just Exact | p >= 0 -> Rational $ (allDigits * 10 ^ p) % 1
        Just Exact | p < 0 -> Rational $ allDigits % (10 ^ (-p))
        _ -> Double $ fromInteger allDigits * 10 ^^ p

fullDecimal10 :: Maybe Exactness -> Parser Real
fullDecimal10 exactness' = do
    integerDigits <- Parsec.many1 $ digit' Decimal
    void dot
    decimalDigits <- Parsec.many $ digit' Decimal
    s <- Parsec.option 0 suffix

    let p = s - toInteger (length decimalDigits)
    let allDigits = parseDigits Decimal (integerDigits ++ decimalDigits)

    return $ case exactness' of
        Just Exact | p >= 0 -> Rational $ (allDigits * 10 ^ p) % 1
        Just Exact | p < 0 -> Rational $ allDigits % (10 ^ (-p))
        _ -> Double $ fromInteger allDigits * 10 ^^ p

uinteger :: Radix -> Parser Integer
uinteger r = parseDigits r <$> Parsec.many1 (digit' r)

prefix :: Radix -> Parser (Maybe Exactness)
prefix r = Parsec.try (radix r *> exactness') <|> Parsec.try (exactness' <* radix r)
    where
        exactness' = Parsec.optionMaybe exactness

inf :: Parser Real
inf = Parsec.string' "+inf.0" $> Double (1/0)
    <|> Parsec.string' "-inf.0" $> Double (-1/0)

nan :: Parser Real
nan = (Parsec.string' "+nan.0" <|> Parsec.string' "-nan.0") $> Double 0/0

infNan :: Parser Real
infNan = inf <|> nan

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

sign :: Parser Sign
sign = positive <|> negative
    where
        positive = Parsec.char '+' $> Positive
        negative = Parsec.char '-' $> Negative

exactness :: Parser Exactness
exactness = Parsec.try exact <|> Parsec.try inexact
    where
        exact = Parsec.string' "#e" $> Exact
        inexact = Parsec.string' "#i" $> Inexact

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

-- Directive

directive :: Parser ()
directive = foldCaseDirective <|> noFoldCaseDirective <?> "directive"

foldCaseDirective :: Parser ()
foldCaseDirective = Parsec.string' "#!fold-case" *> Parsec.putState FoldCase

noFoldCaseDirective :: Parser ()
noFoldCaseDirective = Parsec.string' "#!no-fold-case" *> Parsec.putState NoFoldCase

-- Datum

datum :: Parser Datum
datum = Parsec.try simpleDatum
    <|> Parsec.try compoundDatum
    <|> Parsec.try labeled
    <|> Label <$> Parsec.try label <* Parsec.char '#'

many :: Parser a -> Parser [a]
many p = intertokenSpace *> Parsec.sepEndBy p intertokenSpace

many1 :: Parser a -> Parser [a]
many1 p = intertokenSpace *> Parsec.sepEndBy1 p intertokenSpace

data' :: Parser [Datum]
data' = many datum

data1 :: Parser [Datum]
data1 = many1 datum

label :: Parser Integer
label = Parsec.char '#' *> uinteger Decimal

labeled :: Parser Datum
labeled = do
    label' <- label
    void $ Parsec.char '='
    Labeled label' <$> datum

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

compoundDatum :: Parser Datum
compoundDatum = List <$> Parsec.try list
    <|> Vector <$> Parsec.try vector
    <|> Parsec.try quotation
    <|> Parsec.try quasiquotation
    <|> Parsec.try unquoted
    <|> Parsec.try unquotedSplicing

quotation :: Parser Datum
quotation = Quoted <$> (quote *> datum)

quasiquotation :: Parser Datum
quasiquotation = Quasiquoted <$> (backtick *> datum)

unquoted :: Parser Datum
unquoted = Unquoted <$> (comma *> datum)

unquotedSplicing :: Parser Datum
unquotedSplicing = UnquotedSplicing <$> (commaAt *> datum)

symbol :: Parser String
symbol = identifier

simpleDatum :: Parser Datum
simpleDatum = Boolean <$> Parsec.try boolean
    <|> Number <$> Parsec.try number
    <|> Character <$> Parsec.try character
    <|> String <$> Parsec.try string
    <|> Symbol <$> Parsec.try symbol
    <|> ByteVector <$> Parsec.try bytevector
