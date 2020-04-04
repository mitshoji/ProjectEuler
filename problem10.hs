{-
Summation of primes
   
Problem 10
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

newPrimes :: [Integer]
newPrimes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod`d /= 0) [ x | x <- [2..bound n]]
    where
       bound q = sqrint [1..q] q
       sqrint (r:rs) q | (r * r) <  q = sqrint rs q
                       | (r * r) == q = r
                       | (r * r) >  q = r-1


main :: IO ()
main = print $ sum $ takeWhile ( < 2000000) newPrimes



--Answer: 142913828922
