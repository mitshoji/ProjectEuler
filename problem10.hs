{-
Summation of primes
   
Problem 10
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod`d /= 0) $ takeWhile (\d -> d*d <= n) primes


main :: IO ()
main = print $ sum $ takeWhile ( < 2000000) primes



--Answer: 142913828922
