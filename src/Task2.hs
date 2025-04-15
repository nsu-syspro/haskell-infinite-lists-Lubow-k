{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task2 where

import Prelude hiding (repeat, iterate, filter)

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show :: Show a => Stream a -> String
  show stream = "[" ++ showN 10 stream ++ "]"

showN :: Show a => Int -> Stream a -> String
showN n (Stream x xs) = show x ++ next (n - 1) xs where
  next 0 _ = ""
  next m (Stream y ys) = "," ++ show y ++ next (m - 1) ys


instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Stream x xs) = Stream (f x) (fmap f xs)
  

instance Foldable Stream where
  foldMap :: Monoid m => (a -> m) -> Stream a -> m
  foldMap f (Stream x xs) = f x <> foldMap f xs


-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList v = foldr Stream (repeat v) 

repeat :: a -> Stream a
repeat x = Stream x (repeat x)

iterate :: (a -> a) -> a -> Stream a
iterate f x = Stream x (iterate f v) where v = f x

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (Stream x xs) 
  | f x        = Stream x (filter f xs)
  | otherwise  = filter f xs   

dropFirst :: Stream a -> Stream a
dropFirst (Stream _ xs) = xs

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f seed = Stream value (unfold f newSeed) 
                where (value, newSeed) = f seed

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\a -> (a, a + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold (\(x, y) -> (x, (y, x + y))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = unfold sieve $ dropFirst nats

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream x xs) = (x, filter ((/= 0) . (`mod` x)) xs)
