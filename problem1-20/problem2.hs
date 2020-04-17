{-
Even Fibonacci numbers
   
Problem 2
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
-}

fourMillion :: Integer
fourMillion = 4000000

fibonacci :: [Integer]
fibonacci = 1:2:zipWith (+) fibonacci (tail fibonacci)

evenFibonacci :: [Integer]
evenFibonacci = filter isEven $ takeWhile ( < fourMillion) fibonacci
    where
    -- isEven :: Integer -> Bool
       isEven n = n `mod` 2 == 0

main :: IO ()
main = print $ sum evenFibonacci
