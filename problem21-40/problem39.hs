import Data.List

main :: IO ()
main = print $ snd $ maximum $ map (\p -> ((length.nub $ map reduce $ edges p),p)) [3..1000]

edges :: Int -> [(Int, Int)]
edges p = [(m,n) | m <- lst p, n <- drop m $lst p,
                   p `mod` (2*n^2 + 2*m*n) == 0,
                   (n^2 - m^2) < 2*n*m]
        where
           lst x = takeWhile (\t -> t*t < (x `div` 2)) [1..]

reduce :: (Int, Int) -> (Int, Int)
reduce (x,y) | cd == 1   = (x,y)
             | otherwise = (x `div` cd, y `div` cd)
                         where
                            cd = gcd x y
