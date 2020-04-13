{-
Problem 4
   A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.

   Find the largest palindrome made from the product of two 3-digit numbers.
-}
import Data.List

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x


candidates :: [Int]
candidates = (reverse . sort) [ x * y | x <- [101..999], y <- [x..999]]
-- using head with (reverse.sort) is faster than using last with sort


main :: IO ()
main = print $ head $ filter isPalindrome $ map show candidates


