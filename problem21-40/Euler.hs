module Euler
( primes
, isPrime
, breakToDigits
, divisors
, factorization
) where

import Data.List

primes :: (Integral a, Read a, Show a) => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: (Integral a, Read a, Show a) => a -> Bool
isPrime n = all (\d -> n `mod`d /= 0) $ takeWhile (\d -> d*d <= n) primes

---------------------------------
divisors :: Int -> [Int]
divisors n = divisors' n [ d | d <- [2..n], n `mod` d == 0]

divisors' :: Int -> [Int] -> [Int]
divisors' 1 _ = []
divisors' _ [] = []
divisors' n (d:ds) | n `mod` d == 0 = d : divisors' (n `div` d) (d:ds)
                   | otherwise      = divisors' n ds

factorization :: Int -> [(Int, Int)] 
factorization n = map gcnt $ group $ divisors n
                where
                -- gcnt :: [a] -> (a,b)
                   gcnt (x:xs) = (x, length (x:xs))

---------------------------------
breakToDigits :: (Integral a, Read a, Show a) => a -> [a] 
breakToDigits n = map (read. return) $ show n

