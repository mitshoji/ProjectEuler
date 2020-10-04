import Euler (isPrime, primes)

--This is based on the property of the totient function, that is;
--phi/n = PI(1-1/p), which means numbers whoes prime factors are same
--should have the same phi/n value. Since what we need to find is the number
--that gives the minimum value of phi/n, the number has as many prime factors as possible,
--in ascending order (small to large).
--

main = print $ primeProd $ primesUnder 1000
-- we can bound upper limit with sqrt 1,000,000.

primeProd :: (Integral a) => [a] -> a
primeProd ps = foldProd 1 1000000 ps 
  where
    foldProd acc _ []                       = acc
    foldProd acc bnd (x:xs) | acc * x < bnd = foldProd (acc * x) bnd xs
                            | otherwise     = acc


primesUnder :: (Integral a) => a -> [a]
primesUnder 1 = []
primesUnder n = filter (isPrime) [2..n]



--totient function
--import Data.Ratio
{-
totientByN :: (Integral a) => a -> (Ratio a, a)
totientByN 1 = (1,1)
totientByN n = (phiByN, n)
  where
    phiByN = foldl (\acc x -> acc * (1-(1 % x))) 1 $ primeFactors n
-}

