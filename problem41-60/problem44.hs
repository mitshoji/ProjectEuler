
main :: IO ()
main = print $ head $ findDS [] pentagonal


-- Let be Pi + Pj = S, Pi - Pj = D
-- Then, D < Pi < S , all contained in Pentagonals
-- findDS searches (S-D) which is a member of Pentagnals
findDS :: [Integer] -> [Integer] -> [(Integer, Integer)]
findDS _ []      = []
findDS ds (p:ps) = diff ++ findDS (p:ds) ps
                 where
                    diff = [(d,s) | d <- ds,
                                    s <- takeWhile (< 2*p) ps,
                                    d + s == 2*p,
                                    isPentagonal (s-d) ]
-- pentagonal condition
isPentagonal :: Integer -> Bool
isPentagonal q = q == eq*(3*eq-1) 
    where
       eq = truncate $ (1 + sqrt (1 + 12*(fromIntegral q))) / 6


pentagonal :: [Integer]
pentagonal = [n*(3*n-1) `div` 2 | n <- [1..]]
