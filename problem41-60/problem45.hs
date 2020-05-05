main :: IO ()
main = print $ head pas2H


pas2H :: [Integer]
pas2H = [(p k) `div` 2 | k <- [166..], (p k) == tas2H k]
    where
       p k = k*(3*k-1)    

tas2H :: Integer -> Integer
tas2H k | f `mod` 2 == 1 = f*(f+1)
        | otherwise      = 0
    where
       f = truncate $ sqrt $ fromIntegral (k*(3*k-1))
