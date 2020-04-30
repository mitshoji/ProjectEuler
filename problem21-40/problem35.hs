main :: IO ()
main = print $ length $ filter isCyclPrm pms

pms :: [Integer]
pms = takeWhile ( < 1000000) primes


------------------------------------------------------------------------------------
mkCycle :: [a] -> [[a]]
mkCycle xs = take (length xs) $ cycle xs
           where
              cycle xs = nxs : mkCycle nxs
              nxs = (last xs) : (init xs)


isCyclPrm :: Integer -> Bool
isCyclPrm p = all isPrime $ map read $ mkCycle $ show p


primes :: (Integral a, Num a) => [a]
primes = 2 : 3 : 5 : filter isPrime [7,9..]

isPrime :: (Integral a, Num a) => a -> Bool
isPrime n | n <= 1 = False
          | n == 2 = True
          | n `mod` 2 == 0 = False
          | otherwise = all (\d -> n `mod`d /= 0) $ takeWhile (\d -> d*d <= n) [3,5..]

