{-
Multiples of 3 and 5
   
Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

numList :: [Int]
numList = [ n | n <- [1..999], n `isMultipuleOf` 3 || n `isMultipuleOf` 5]
        where
         --isMultipuleOf :: Int -> Int -> Bool
           isMultipuleOf n m = n `mod` m == 0

main :: IO ()
main = print $ sum numList

-- Answer: 233168
