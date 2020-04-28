import Data.Char

main :: IO ()
main = print $ sum $ filter eval [3..(7*fact 9)]


eval :: Int -> Bool
eval n = n == (sumup $ btg n)
     where sumup = foldr (\n m -> m + fact n) 0 

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

btg :: Int -> [Int]
btg = map digitToInt.show





