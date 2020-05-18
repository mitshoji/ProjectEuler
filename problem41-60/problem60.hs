import Data.List

main :: IO ()
main = do 
       let lst = find (\xs -> length xs == 5) $ findGroup [] primes
           sm  = fmap sum lst
       print (sm, lst)

findGroup :: [[Integer]] -> [Integer] -> [[Integer]]
findGroup [] (x:xs) = [x] : findGroup [[x]] xs
findGroup ps (x:xs) = fps' ++ findGroup fps xs
                          where fps = ps ++ fps' 
                                fps' = [x] : filter allPairs (map (x:) ps) 

allPairs :: [Integer] -> Bool
allPairs [p]           = True
allPairs [p,q]         = pairs p q
allPairs (p:q:qs) = all (pairs p) (q:qs) && all (pairs q) qs

pairs :: Integer-> Integer -> Bool
pairs p q = all isPrime [pandq, qandp]
    where
       pandq = read $ concatMap show [p,q]
       qandp = read $ concatMap show [q,p]

primes :: [Integer]
primes = 3:7:filter isPrime [11,13..]

isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod` d /= 0) $ takeWhile (\d -> d*d <= n) [3,5..]
