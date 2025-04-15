{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (Stream(..), repeat)
import Data.Ratio (Ratio, numerator)

import Prelude hiding (repeat)

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]

-- Define the power series corresponding to x
x :: Num a => Series a
x = Series $ Stream 0 (Stream 1 (repeat 0))


instance Num a => Num (Series a) where

  fromInteger :: Num a => Integer -> Series a
  fromInteger n = Series $ Stream (fromInteger n) (repeat 0)

  negate :: Num a => Series a -> Series a
  negate (Series c) = Series $ fmap negate c

  (+) :: Num a => Series a -> Series a -> Series a
  (+) (Series c1) (Series c2) = Series $ zipWithStream (+) c1 c2

  (*) :: Num a => Series a -> Series a -> Series a
  (*) (Series c1) b@(Series c2) = Series $ Stream first rest where
    (Stream a0 restA) = c1  
    (Stream b0 restB) = c2 
    first = a0 * b0
    rest = coefficients (a0 *: Series restB + Series restA * b) 

  abs :: Num a => Series a -> Series a
  abs (Series c) = Series $ fmap abs c

  signum :: Num a => Series a -> Series a
  signum (Series c) = Series $ fmap signum c

zipWithStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithStream f (Stream y ys) (Stream z zs) = Stream (f y z) (zipWithStream f ys zs)

-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
(*:) n (Series c) = Series $ fmap (* n) c 


instance Fractional a => Fractional (Series a) where

  fromRational :: Fractional a => Rational -> Series a
  fromRational n = Series $ Stream (fromRational n) (repeat 0)

  (/) :: Fractional a => Series a -> Series a -> Series a
  (/) (Series c1) b@(Series c2) = Series $ Stream first rest where
    (Stream a0 restA) = c1  
    (Stream b0 restB) = c2 
    first = a0 / b0
    rest = coefficients ((Series restA - first *: Series restB) / b)

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen (Series c) = fmap numerator c

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen (1 / (1 - x))

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen (1 / ((1 - x) * (1 - x)))

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen (x / (1 - x - x * x))
