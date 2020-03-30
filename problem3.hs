{-
Largest prime factor
   
Problem 3
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

dividend :: Integer
dividend = 600851475143

primes :: [Integer]
primes = 2 : sieve [3,5..]
    where
     --sieve :: [Integer] -> [Integer]
       sieve (x:xs) = x : sieve [ p | p <- xs, p `mod` x /= 0]


primeList :: Integer -> [Integer]
primeList m = takeWhile (\x -> x*x < m) primes



findDivisor :: Integer -> [Integer] -> Integer
findDivisor m [] = m 
findDivisor m (d:ds)
    | m `mod` d /= 0 = findDivisor m ds
    | m `mod` d == 0 = findDivisor m' (primeList m')
                         where
                            m' = m `div` d


main :: IO ()
main = print $ findDivisor dividend (primeList dividend)



-- Answer: 6857
