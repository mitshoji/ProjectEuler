import Data.List

main :: IO ()
main = print $ candidates


candidates :: [(Integer, Integer, Integer)]
candidates = [ (x,y,z) | d <- [9,18..3330],
                         let bnd = 9999 - 2*d,                        
                         x <- filter isPrime [1000..bnd],
                         let y = x + d,
                         let z = x + 2*d,
                         isPrime y, isPrime z,
                         isPerm x y, isPerm x z]

isPerm :: Integer -> Integer -> Bool
isPerm n m = ms `elem` perm n
    where
       ms     = show m
       perm n = permutations $ show n


isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod` d /= 0) $ takeWhile (\d -> d*d <= n) (2:[3,5..])

