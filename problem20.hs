import Data.Char

main :: IO ()
main = print $ digitSum $ factorial 100


digitSum :: Integer -> Int
digitSum = sum. (map digitToInt). show 


factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)
