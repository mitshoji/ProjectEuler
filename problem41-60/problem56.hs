import Data.Char

main :: IO ()
main = print $ maximum [ digitSum q | a <- [1..100], r <- [1..100], let q = a^r]


digitSum :: Integer -> Int
digitSum = sum.(map digitToInt).show
