{-
Large sum
   
Problem 13
Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

see problem13_numbers.txt

-}

numsList :: IO [String]
numsList = fmap lines $ readFile "problem13_numbers.txt"


main :: IO ()
main = numsList >>= print .(take 10). show. sum.(map read)
