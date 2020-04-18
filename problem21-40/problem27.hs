import Euler

formula :: Int -> Int -> [Int]
formula a b = [n*n + a*n + b |n <- [0..b]]

nums :: [[Int]]
nums = map (takeWhile proper) [formula a b | b <- pms, a <- (*) <$> [-1,1] <*> [0..b]]
     where
        proper = and isPrime  ( > 1)

pms :: [Int]
pms = takeWhile (< 1000) primes
