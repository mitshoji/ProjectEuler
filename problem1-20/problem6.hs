{-
Sum square difference
   
Problem 6
The sum of the squares of the first ten natural numbers is,

1^2+2^2+...+10^2=385
The square of the sum of the first ten natural numbers is,

(1+2+...+10)^2=55^2=3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025-385=2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

sumofSquares :: [Int] -> Int
sumofSquares = foldl (\x y -> x + y*y) 0 

squareSum :: [Int] -> Int
squareSum = (^2).sum

delta :: [Int] -> Int
delta ns = squareSum ns -  (sumofSquares ns)

main :: IO ()
main = print $ delta [1..100]


