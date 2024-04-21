module Crafty.Datum where

import Prelude hiding (Rational, Real)
import Data.Word (Word8)
import GHC.Real (Ratio((:%)))
import qualified GHC.Real

-- Rational

data Rational
    -- TODO: Replace with Data.Rational
    = Ratio Integer Integer
    | Double Double
    | Integer Integer
    deriving (Show)

instance Num Rational where
    a + b = case (a, b) of
        (Integer x, Integer y) -> Integer $ x + y
        (Integer _, Double _) -> b + a
        (Integer _, Ratio _ _) -> b + a

        (Double x, Integer y) -> Double $ x + fromInteger y
        (Double x, Double y) -> Double $ x + y
        (Double x, Ratio y z) -> Double $ x + fromRational (y :% z)

        (Ratio _ _, Integer z) -> a + Ratio z 1
        (Ratio _ _, Double _) -> b + a
        (Ratio x y, Ratio x' y') -> makeRatio (x + x') (y + y')

    a * b = case (a, b) of
        (Integer x, Integer y) -> Integer $ x * y
        (Integer _, Double _) -> b * a
        (Integer _, Ratio _ _) -> b * a

        (Double x, Integer y) -> Double $ x * fromInteger y
        (Double x, Double y) -> Double $ x * y
        (Double x, Ratio y z) -> Double $ x * fromRational (y :% z)

        (Ratio _ _, Integer z) -> a * Ratio z 1
        (Ratio _ _, Double _) -> b * a
        (Ratio x y, Ratio x' y') -> makeRatio (x * x') (y * y')
    
    abs r = case r of
        Integer x -> Integer $ abs x
        Double x -> Double $ abs x
        Ratio x y -> makeRatio (abs x) y
    
    signum r = case r of
        Integer x -> Integer $ signum x
        Double x -> Double $ signum x
        Ratio x _ -> Integer $ signum x
    
    fromInteger = Integer

    negate r = case r of
        Integer x -> Integer $ -x
        Double x -> Double $ -x
        Ratio x y -> Ratio (-x) y

instance Eq Rational where
    a == b = case (a, b) of
        (Integer x, Integer y) -> x == y
        -- FIXME?
        (Integer _, Double _) -> False
        (Integer x, Ratio _ _) -> Ratio x 1 == b

        (Double _, Integer _) -> b == a
        (Double x, Double y) -> x == y
        (Double x, Ratio y z) -> x == fromRational (y :% z)

        (Ratio _ _, Integer x) -> a == Ratio x 1
        -- FIXME?
        (Ratio _ _, Double _) -> False
        -- Assumes simplest terms
        (Ratio x y, Ratio x' y') -> x == x' && y == y'

instance Ord Rational where
    x <= y = toRational x <= toRational y

instance GHC.Real.Real Rational where
    toRational r = case r of
        Ratio x y -> x :% y
        Integer x -> toRational x
        Double x -> toRational x

instance Fractional Rational where
    fromRational (x :% y) = Ratio x y
    
    recip r = case r of
        Integer x -> makeRatio 1 x
        Double x -> Double $ recip x
        Ratio x y -> makeRatio y x

instance RealFrac Rational where
    properFraction r = case r of
        Integer x -> properFraction $ Ratio x 1
        Double x -> let (n, f) = properFraction x in (n, Double f)
        Ratio x y -> let (n, x' :% y') = properFraction (y :% x) in (n, Ratio x' y')

-- Real

data Real
    = Nan
    | PositiveInf
    | NegativeInf
    | Rational Rational
    deriving (Show)

instance Num Real where
    a + b = case (a, b) of
        (Nan, _) -> Nan
        (_, Nan) -> Nan

        (PositiveInf, NegativeInf) -> Nan
        (PositiveInf, _) -> PositiveInf

        (NegativeInf, PositiveInf) -> Nan
        (NegativeInf, _) -> NegativeInf

        (Rational x, Rational y) -> Rational $ x + y
        (Rational _, _) -> b + a
    
    a * b = case (a, b) of
        (Nan, _) -> Nan
        (_, Nan) -> Nan

        (PositiveInf, PositiveInf) -> PositiveInf
        (PositiveInf, NegativeInf) -> NegativeInf
        (PositiveInf, Rational r) -> if r < 0 then NegativeInf else PositiveInf

        (NegativeInf, PositiveInf) -> NegativeInf
        (NegativeInf, NegativeInf) -> PositiveInf
        (NegativeInf, Rational r) -> if r < 0 then PositiveInf else NegativeInf

        (Rational x, Rational y) -> Rational $ x * y
        (Rational _, _) -> b * a
    
    abs r = case r of
        Nan -> Nan
        PositiveInf -> PositiveInf
        NegativeInf -> NegativeInf
        Rational x -> Rational $ abs x
    
    signum r = case r of
        Nan -> Nan
        PositiveInf -> 1
        NegativeInf -> -1
        Rational x -> Rational $ signum x

    fromInteger = Rational . fromInteger

    negate r = case r of
        Nan -> Nan
        PositiveInf -> NegativeInf
        NegativeInf -> PositiveInf
        Rational x -> Rational $ -x

instance Eq Real where
    a == b = case (a, b) of
        (PositiveInf, PositiveInf) -> True
        (NegativeInf, NegativeInf) -> True
        (Rational x, Rational y) -> x == y
        _ -> False

instance Ord Real where
    a <= b = a == b || case (a, b) of
        (NegativeInf, PositiveInf) -> True
        (Rational x, Rational y) -> x < y
        _ -> False

instance Fractional Real where
    fromRational = Rational . fromRational

    recip r = case r of
        Nan -> Nan
        PositiveInf -> 0
        NegativeInf -> 0
        Rational x -> Rational $ recip x

-- Complex

-- TODO: Wrap in Number and hide constructors?
data Complex
    = Rectangular Real Real
    | Polar Real Real
    | Real Real
    deriving (Show)

-- Datum

data Datum
    = Boolean Bool
    | Number Complex
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

-- Construct a ratio in simplest terms
makeRatio :: Integer -> Integer -> Rational
makeRatio x y = let d = gcd x y in Ratio (x `div` d) (y `div` d)

-- makeByte :: Complex -> Maybe Word8
-- makeByte c = case c of
