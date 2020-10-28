import qualified Data.Map as Map
type Table = Map.Map Int Bool

lmax :: Int
lmax = 1500000

main :: IO ()
main = print $ length $ foldWithTable mapEvalSides Map.empty [12,14..lmax]



foldWithTable :: (Int -> Table -> (Bool, Table)) -> Table -> [Int] -> [Int]
foldWithTable _ _ [] = []
foldWithTable f tbl (l:ls) | fst $ f l tbl = l : foldWithTable f nwtbl ls
                           | otherwise     = foldWithTable f nwtbl ls
  where
    nwtbl = snd $ f l tbl


mapEvalSides :: Int -> Table -> (Bool, Table)
mapEvalSides l tbl = case Map.lookup l tbl of
                       Just b -> (b, tbl)
                       Nothing  -> case eval l of
                                    0         -> (False, tbl')
                                    1         -> (True, tbl'')
                                    otherwise -> (False, nwtbl) 
  where
    nwtbl = revTable l tbl
    tbl'  = Map.insert l False tbl
    tbl'' = Map.insert l True tbl


revTable :: Int -> Table -> Table
revTable l tbl = rvtbl l tbl (takeWhile (< lmax) $ map (*l) [1,2..])
  where
    rvtbl l tbl []     = tbl
    rvtbl l tbl (z:zs) = rvtbl l tbl' zs
      where
        tbl' = Map.insert z False tbl


eval :: Int -> Int
eval l =  length $ filter (\z -> isSqr $ f l z) [lwz .. upz]
  where
    f l z   = 2*z^2 - (l - z)^2
    isSqr n = (floor $ sqrt $ fromIntegral n)^2 == n
    lwz     = ceiling $ (fromIntegral l)/(1 + sqrt 2)
    upz     = l `div` 2 - 1

