import Data.List

main :: IO ()
main = do 
       let prmRatio = zip (totalPrimes 0 [1..]) (iterate (+ 4) 1)
           x = fmap snd $ find (\(x,y) -> 10 * x < y) $ tail prmRatio 
       print $ fmap (\n -> (n + 1) `div` 2) x


totalPrimes :: Int -> [Int] -> [Int]
totalPrimes ps (n:ns) = ps : totalPrimes (ps + cp) ns
    where
       cp = cntPrime n


cntPrime :: Int -> Int
cntPrime n = length $ filter isPrime [x,y,z]
    where
       x = (2 * n + 1)^2 - 2 * n
       y = (2 * n + 1)^2 - 4 * n
       z = (2 * n + 1)^2 - 6 * n

isPrime :: Int -> Bool
isPrime n = all (\d -> n `mod` d /= 0) $ takeWhile (\d -> d*d <= n) (2:[3,5..])
