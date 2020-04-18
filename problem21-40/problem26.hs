main :: IO ()
main = print $ snd $ maximum $ zip (map (length.rcycle) [1..999]) [1..999]

rcycle :: Int -> [Int]
rcycle d = residue [] 1 d
          where
            residue rs n d | n  `mod` d == 0 = rs
                           | r `elem` rs     = rs 
                           | otherwise       = residue (r:rs) (r*10) d
                                             where r = n `mod` d

