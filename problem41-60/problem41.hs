import Euler
import Data.List

main :: IO ()
main = print $ nPandigitalPrimes "7654321" 


nPandigitalPrimes :: String -> Int
nPandigitalPrimes []     = error "There is no pandigital prime"
nPandigitalPrimes (c:cs) | plist == [] = nPandigitalPrimes cs
                         | otherwise   = maximum plist 
                           where
                              plist = filter isPrime $ map read $ permutations (c:cs)
