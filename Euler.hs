module Euler
( primes
, breakToDigits
) where

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod`d /= 0) $ takeWhile (\d -> d*d <= n) primes


breakToDigits :: (Integral a, Read a, Show a) => a -> [a] 
breakToDigits n = map (read. return) $ show n

