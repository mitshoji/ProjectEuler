import Data.List
import Euler

main :: IO ()
main = print $ sum $ take 11 $ filter isTruncatable $ drop 4 primes
-- drop 4 is to pick out first 4 primes; 2,3,5,7

isTruncatable :: Int -> Bool
isTruncatable n = all isPrime $ truncateLR n

truncateLR :: Int -> [Int]
truncateLR n = (tail $ truncateL n) ++ (truncateR n) 

truncateL :: Int -> [Int]
truncateL = (map read). init. tails. show
-- truncateL 247 = [247,47,7]
-- we need init since tails returns empty element at last,
-- like [247,47,7,""]

truncateR :: Int -> [Int]
truncateR = (map read). tail. inits. show
-- truncateR 247 = [2,24,247]
-- In truncateL, we need tail as we do in truncateR for the same reason.
