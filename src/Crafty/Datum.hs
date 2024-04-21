module Crafty.Datum where

import Prelude hiding (Rational, Real)
import Data.Complex hiding (Complex)
import qualified Data.Complex
import Data.Word (Word8)
import qualified GHC.Real
import GHC.Stack (HasCallStack)
import Data.Maybe (fromJust)
import GHC.Real (Ratio((:%)))

-- Real

data Real
    -- Used for all inexact values, including Nan and infinities
    -- Snarfing from Double because it's already IEEE 754 compliant
    = Double Double
    -- Used for all exact values, including integers, where the denominator is one
    | Rational GHC.Real.Rational
    deriving (Show)

instance Num Real where
    a + b = case (a, b) of
        (Double x, Double y) -> Double $ x + y
        (Double x, Rational y) -> Double $ x + fromRational y
        (Rational x, Double y) -> Double $ fromRational x + y
        (Rational x, Rational y) -> Rational $ x + y

    a * b = case (a, b) of
        (Double x, Double y) -> Double $ x * y
        (Double x, Rational y) -> Double $ x * fromRational y
        (Rational x, Double y) -> Double $ fromRational x * y
        (Rational x, Rational y) -> Rational $ x * y

    abs r = case r of
        Double x -> Double $ abs x
        Rational x -> Rational $ abs x

    signum r = case r of
        Double x -> Double $ signum x
        Rational x -> Rational $ signum x

    fromInteger = Rational . fromInteger

    negate r = case r of
        Double x -> Double $ -x
        Rational x -> Rational $ -x

-- See note in R7RS spec about transitivity of equality
instance Eq Real where
    a == b = case (a, b) of
        (Double x, Double y) -> x == y
        (Rational x, Rational y) -> x == y

        (Rational x, Double y)
            | isInfinite y -> False
            | isNaN y -> False
            | otherwise -> x == toRational y
        (Double _, Rational _) -> b == a

instance Ord Real where
    a <= b = case (a, b) of
        (Double x, Double y) -> x <= y
        (Rational x, Rational y) -> x <= y

        (Rational x, Double y) -> fromRational x <= y
        (Double x, Rational y) -> x <= fromRational y

instance Fractional Real where
    fromRational = Rational
    recip r = case r of
        Double x -> Double $ recip x
        Rational x -> Rational $ recip x

-- Complex

-- TODO: Wrap in Number and hide constructors?
data Number
    = Complex (Data.Complex.Complex Real)
    -- TODO: Maybe remove this constructor, and use Complex with an exact (Rational) zero imaginary part?
    | Real Real
    deriving (Show, Eq)

-- FIXME: Review this, check to see if there are any unnecessary conversions from Rational to Double
instance Num Number where
    a + b = case (a, b) of
        (Complex (x :+ y), Complex (x' :+ y')) -> Complex $ (x + x') :+ (y + y')
        (Complex _, Real x) -> a + Complex (x :+ 0)
        (Real _, Complex _) -> b + a
        (Real x, Real y) -> Real $ x + y

    a * b = case (a, b) of
        (Complex (x :+ y), Complex (x' :+ y')) -> Complex $ (x * x' - y * y') :+ (x * y' + y * x')
        (Complex _, Real x) -> a * Complex (x :+ 0)
        (Real _, Complex _) -> b * a
        (Real x, Real y) -> Real $ x * y

    abs c = case c of
        Complex (Double x :+ Double y) -> let a = abs (x :+ y) in Complex $ Double (realPart a) :+ Double (imagPart a)
        Complex (Double x :+ Rational y) -> abs $ Complex (Double x :+ Double (fromRational y))
        Complex (Rational x :+ Double y) -> abs $ Complex (Double (fromRational x) :+ Double y)
        Complex (Rational x :+ Rational y) -> abs $ Complex (Double (fromRational x) :+ Double (fromRational y))
        Real x -> Real $ abs x

    signum c = case c of
        Complex (Double x :+ Double y) -> let s = signum (x :+ y) in Complex $ Double (realPart s) :+ Double (imagPart s)
        Complex (Double x :+ Rational y) -> signum . Complex $ Double x :+ Double (fromRational y)
        Complex (Rational x :+ Double y) -> signum . Complex $ Double (fromRational x) :+ Double y
        Complex (Rational x :+ Rational y) -> signum . Complex $ Double (fromRational x) :+ Double (fromRational y)
        Real x -> Real $ signum x

    fromInteger = Real . fromInteger

    negate c = case c of
        Complex (x :+ y) -> Complex $ (-x) :+ (-y)
        Real x -> Real $ negate x

instance Fractional Number where
    fromRational = Real . Rational

    recip n = case n of
        Complex (Double x :+ Double y) -> let r = recip (x :+ y) in Complex $ Double (realPart r) :+ Double (imagPart r)
        Complex (Double x :+ Rational y) -> recip . Complex $ Double x :+ Double (fromRational y)
        Complex (Rational x :+ Double y) -> recip . Complex $ Double (fromRational x) :+ Double y
        Complex (Rational x :+ Rational y) -> recip . Complex $ Double (fromRational x) :+ Double (fromRational y)
        Real r -> Real $ recip r

isExact :: Number -> Bool
isExact n = case n of
    Real (Rational _) -> True
    Complex (Rational _ :+ Rational _) -> True
    _ -> False

inexact :: Number -> Number
inexact n = case n of
    Real (Rational r) -> Real . Double $ fromRational r
    Complex (Rational x :+ Rational y) -> Complex $ Double (fromRational x) :+ Double (fromRational y)
    _ -> n

exact :: Number -> Maybe Number
exact n = case n of
    Real (Double d)
        | isInfinite d -> Nothing
        | isNaN d -> Nothing
        | otherwise -> Just . Real . Rational $ toRational d
    Complex (Double x :+ Double y) -> Just . Complex $ Rational (toRational x) :+ Rational (toRational y)
    Complex (Double x :+ y@(Rational _)) -> Just . Complex $ Rational (toRational x) :+ y
    Complex (x@(Rational _) :+ Double y) -> Just . Complex $ x :+ Rational (toRational y)
    _ -> Just n

exact' :: HasCallStack => Number -> Number
exact' = fromJust . exact

makeRectangular :: Real -> Real -> Number
makeRectangular a b = Complex $ a :+ b

makePolar :: Real -> Real -> Number
makePolar r t = case (r, t) of
    (Double r', Double t') -> let c = mkPolar r' t' in Complex $ Double (realPart c) :+ Double (imagPart c)
    (Double r', Rational t') -> makePolar (Double r') (Double $ fromRational t')
    (Rational r', Double t') -> makePolar (Double $ fromRational r') (Double t')
    -- TODO: Exact polar complex numbers?
    (Rational r', Rational t') -> makePolar (Double $ fromRational r') (Double $ fromRational t')

asInteger :: Number -> Maybe Integer
asInteger n = case n of
    Real (Rational (x :% y)) | y == 1 -> Just x
    _ -> Nothing

-- Datum

data Datum
    = Boolean Bool
    | Number Number
    | Character Char
    | String String
    | Symbol String
    -- TODO: Use appropriate collection types (e.g. fixed-size containers for vector/bytevector)
    | ByteVector [Word8]
    | List [Datum]
    | Vector [Datum]
    | Labeled Integer Datum
    | Label Integer
    | Quoted Datum
    | Quasiquoted Datum
    | Unquoted Datum
    | UnquotedSplicing Datum
    deriving (Show, Eq)

-- Utilities

-- makeByte :: Complex -> Maybe Word8
-- makeByte c = case c of
