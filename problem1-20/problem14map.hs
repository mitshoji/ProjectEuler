{-
Longest Collatz sequence
   
Problem 14
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.


-}

import qualified Data.Map as Map

type Key = Int
type Value = Int
type Table = Map.Map Key Value

main :: IO ()
main = print $ snd $ maximum $ colseq [1..1000000] Map.empty

------------------------------------------------------------

colseq :: [Key] -> Table -> [(Value, Key)]
colseq [] _       = []
colseq (k:ks) tbl = (val,k) : colseq ks tbl'
                    where
                       (val, tbl') = collatz k 0 tbl 


collatz :: Key -> Value -> Table -> (Value, Table)
collatz 1 val tbl  = (val + 1, tbl)
collatz k val tbl = case Map.lookup k tbl of
                    Just v  -> (val + v , tbl)
                    Nothing -> collatz (col k) (val + 1) newtbl
                    where
                       newtbl = Map.insert k (fst $ collatz k 0 tbl) tbl


col :: Key -> Key
col 1 = 1
col k | k `mod` 2 == 0 = k `div` 2
      | otherwise      = 3 * k + 1


