import Data.Array

pentagonals :: [Integer]
pentagonals = [j * (3 * j + a) `quot` 2 | j <- [1..], a <- [-1,1]]


cache :: Array Integer Integer
cache = array (0,100000) [(x, partition x) | x <- [0..100000]]


partition :: Integer -> Integer
partition n | n <= 1 = 1
            | otherwise = sum [s * cache ! (n-p) | (s,p) <- zip (cycle [1,1,-1,-1])
                                                   (takeWhile (<= n) pentagonals)]


main :: IO()
main = print $ head [i | (i,p) <- assocs cache, p `mod` 1000000 == 0]
