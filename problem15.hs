{-
Lattice paths
   
Problem 15
Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


How many such routes are there through a 20x20 grid?
-}

main :: IO ()
main = print $ latticePath 20


latticePath :: Integer -> Integer
latticePath n = fact (n+n) `div` (fact n)^2
                where
                   fact 1 = 1
                   fact n = n * fact (n-1) 

--Answer: 137846528820
