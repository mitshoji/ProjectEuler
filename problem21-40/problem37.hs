import Data.List
import Euler

main :: IO ()
main = print $ sum $ take 11 $ filter isTruncatable $ drop 4 primes
-- drop 4 is to pick out first 4 primes; 2,3,5,7

isTruncatable :: Int -> Bool
isTruncatable n = all isPrime $ shift n

shift :: Int -> [Int]
shift n = (shiftL n) ++ (tail $ shiftR n) 

shiftR :: Int -> [Int]
shiftR = (map read). init. tails. show
-- shiftR 247 = [247,47,7]
-- we need init since tails returns empty element at last,
-- like [247,47,7,""]

shiftL :: Int -> [Int]
shiftL = (map read). tail. inits. show
-- shiftL 247 = [2,24,247]
-- In shiftL, we need tail as we do in shiftR for the same reason.
