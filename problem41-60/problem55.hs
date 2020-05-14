
main :: IO ()
main = print $ length $ filter isLychrel [1..10000] 

isLychrel :: Integer -> Bool
isLychrel n = not $ isPalindrome (last $ lychrel n 0)

lychrel :: Integer -> Int -> [Integer]
lychrel n 0                    = n : lychrel (next n) 1
lychrel n 50                   = [next n]
lychrel n cnt | isPalindrome n = [n]
              | otherwise      = n : lychrel (next n) (cnt + 1)

next :: Integer -> Integer
next n = n + (read $ reverse $ show n)

isPalindrome :: Integer -> Bool
isPalindrome n = (read $ reverse $ show n) == n
