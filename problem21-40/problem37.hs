import Data.List
import Euler

main :: IO ()
main = print $ sum $ take 11 $ filter isTruncatable $ drop 4 primes
-- drop 4 is to pick out first 4 primes; 2,3,5,7

isTruncatable :: Int -> Bool
isTruncatable n = all isPrime $ truncate n

truncate :: Int -> [Int]
truncate n = (truncateL n) ++ (tail $ truncateR n) 

truncateR :: Int -> [Int]
truncateR = (map read). init. tails. show
-- truncateR 247 = [247,47,7]
-- we need init since tails returns empty element at last,
-- like [247,47,7,""]

truncateL :: Int -> [Int]
truncateL = (map read). tail. inits. show
-- truncateL 247 = [2,24,247]
-- In truncateL, we need tail as we do in truncateR for the same reason.
