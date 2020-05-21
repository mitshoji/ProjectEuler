import qualified Data.Map as Map
import Data.List

type Key   = String
type Value = (String, Int)
type Table = Map.Map Key [Value]

main :: IO ()
main = print $ fmap (sum.(map (read.fst))) 
             $ find (\xs -> (length xs == 6) && isLoop xs) 
             $ search octagonal valTable
    where
       isLoop vs = hd == lst
           where hd  = drop 2 $ fst $ head vs
                 lst = take 2 $ fst $ last vs


search :: [[Value]] -> Table -> [[Value]]
search [] _            = []
search (val:vals) tbl  = case Map.lookup (keyOf val) tbl of
                               Nothing ->      val:search vals tbl
                               Just ys -> if   val' == [] 
                                          then val:search vals tbl
                                          else search vals' tbl
                                 where 
                                    val'  = filter nodups $ connect val ys
                                    vals' = val' ++ search vals tbl
        where
           keyOf vs         = drop 2 $ fst $ head vs
           connect vs ys = [(y:vs) | y <- ys] 
           nodups vs     = ndp $ sort $ map snd vs
                          where
                             ndp [x]               = True
                             ndp (x:y:ys) | x /= y = ndp (y:ys)
                                          | x == y = False


valTable :: Table
valTable = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) poligonal 


poligonal :: [(Key,Value)] 
poligonal = [ (k,(fs, m)) | m <- [3..7], n <- [1..145], 
                            let fs = show $ f m n, 
                            let k = take 2 fs, 
                            length fs == 4, fs !! 2 /= '0']
    where
       f m n  | m == 3 = n*(n+1) `div` 2
              | m == 4 = n*n 
              | m == 5 = n*(3*n-1) `div` 2
              | m == 6 = n*(2*n-1)
              | m == 7 = n*(5*n-3) `div` 2
              | otherwise = error "invalid input"

octagonal :: [[Value]]
octagonal = [ [(octs,8)] | n <- [1..58], 
                           let fv = n*(3*n-2), 
                           let octs = show fv, 
                           length octs == 4, octs !! 2 /= '0']
