module Euler
( primes
, isPrime
, breakToDigits
, divisors
, factorization
, divsSum
) where

import qualified Data.Map as Map
import Data.List

primes :: (Integral a, Num a) => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: (Integral a, Num a) => a -> Bool
isPrime n | n <= 1    = False
          | otherwise = all (\d -> n `mod`d /= 0) $ takeWhile (\d -> d*d <= n) [2..]

---------------------------------
divisors :: (Integral a, Num a) => a -> [a]
divisors n = divisors' n [ d | d <- [2..n], n `mod` d == 0]

divisors' :: (Integral a, Num a) => a -> [a] -> [a] 
divisors' 1 _ = []
divisors' _ [] = []
divisors' n (d:ds) | n `mod` d == 0 = d : divisors' (n `div` d) (d:ds)
                   | otherwise      = divisors' n ds

factorization :: (Integral a, Num a) => a -> [(a, Int)] 
factorization n = map gcnt $ group $ divisors n
                where
                -- gcnt :: [a] -> (a,b)
                   gcnt (x:xs) = (x, length (x:xs))

divsSum :: (Integral a, Num a) => a -> a
divsSum n = (calcd $ factorization n) - n

calcd :: (Integral a, Num a) => [(a, Int)] -> a 
calcd = foldr (\x y -> y * f x) 1
      where
      -- f :: (Int, Int) -> Int
         f (p,r) = (p^(r+1) - 1) `div` (p-1)

---------------------------------
breakToDigits :: (Integral a, Read a, Show a) => a -> [a] 
breakToDigits n = map (read. return) $ show n

