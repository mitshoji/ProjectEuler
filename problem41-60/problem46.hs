import Data.List

main :: IO ()
main = print $ find (not. gldbCnj) oddComp 

gldbCnj :: Integer -> Bool
gldbCnj n = any isSqr $ map (\p -> (n - p) `div` 2) $ takeWhile ( < n) primes

isSqr :: Integer -> Bool
isSqr n = n == sq*sq
    where sq = floor $ sqrt $ fromIntegral n

oddComp :: [Integer]
oddComp = filter (not. isPrime) [3,5..]

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime p | p <= 1    = False
          | otherwise = all (\d -> p `mod` d /= 0) $ takeWhile (\d -> d*d <= p) [2..] 
