
main :: IO ()
main = print $ (\d -> d - 1). length $ takeWhile (> 2) $ map snd farey

farey :: [(Int, Int)]
farey = (1,3):(4000,11999):zipWith sub farey (drop 1 farey)
  where
    sub (a,b) (c,d) = (p,q)
      where 
        p = ((n + b) `div` d) * c - a
        q = ((n + b) `div` d) * d - b
        n = 12000



