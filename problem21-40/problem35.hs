import Euler
import Data.Char

main :: IO ()
main = print $ length $ filter isCyclPrm pms

pms :: [Integer]
pms = takeWhile ( < 1000000) primes


mkCycle :: [a] -> [[a]]
mkCycle xs = take (length xs) $ cycle xs
           where
              cycle xs = nxs : mkCycle nxs
              nxs = (last xs) : (init xs)


remg :: [Int] -> Int
remg = foldl1 (\x y ->10*x + y)


isCyclPrm :: Integer -> Bool
isCyclPrm p = all isPrime $ map remg ps
            where
               ps = mkCycle $ map digitToInt $ show p
