module Crafty.Datum where

import Prelude hiding (Rational, Real)
import Data.Word (Word8)
import GHC.Real (Ratio((:%)))
import qualified GHC.Real

data Rational
    -- TODO: Replace with Data.Rational
    = Ratio Integer Integer
    | Double Double
    | Integer Integer
    deriving (Show)

data Real
    = Nan
    | PositiveInf
    | NegativeInf
    | Rational Rational
    deriving (Show, Eq)

-- TODO: Hide constructors?
data Complex
    = Rectangular Real Real
    | Polar Real Real
    | Real Real
    deriving (Show, Eq)

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

-- Instances

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

-- Utilities

-- Construct a ratio in simplest terms
makeRatio :: Integer -> Integer -> Rational
makeRatio x y = let d = gcd x y in Ratio (x `div` d) (y `div` d)

-- makeByte :: Complex -> Maybe Word8
-- makeByte c = case c of
