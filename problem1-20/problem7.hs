{-
10001st prime
   
Problem 7
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

primes :: [Int]
primes = 2:sieve [3,5..]
    where
    -- sieve :: [Int] -> [Int]
       sieve (x:xs) = x : sieve [p | p <- xs, p `mod` x /= 0]

--primes が遅いので違う方法を試してみる


newPrimes :: [Int]
newPrimes = 2 : filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = all (\d -> n `mod`d /= 0) [ x | x <- [2..bound n]]
    where
    -- bound :: Int -> Int
       bound q = sqrint [1..q] q
    -- sqrint :: [Int] -> Int
       sqrint (r:rs) q | (r * r) <  q = sqrint rs q
                       | (r * r) == q = r
                       | (r * r) >  q = r-1


main :: IO ()
main = print $ (!!) newPrimes 10000

