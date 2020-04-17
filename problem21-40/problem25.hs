import Data.Char

main :: IO ()
main = print $ 1 + (length $ takeWhile (\n -> numLength n < 1000) fib)


fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)


numLength :: Integer -> Int
numLength = length. (map digitToInt). show
