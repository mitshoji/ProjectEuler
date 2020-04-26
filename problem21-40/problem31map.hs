import qualified Data.Map as Map

type Key = (Int,[Int])
type Value = Integer
type Table = Map.Map Key Value


main :: IO ()
main = print $ fst $ change 200 coins Map.empty 


coins :: [Int]
coins = [200,100,50,20,10,5,2,1]


change :: Int -> [Int] -> Table -> (Value, Table)
change 0 _ tbl             = (1, tbl)
change _ [] tbl            = (0, tbl)
change prc css@(c:cs) tbl
                 | prc < 0 = (0, tbl)
                 |otherwise = case Map.lookup (prc,css) tbl of
                              Just prc -> (prc, tbl)
                              Nothing  -> (val, newtbl)
                                where
                                   val            = val' + val''
                                   (val', tbl')   = change prc cs tbl
                                   (val'', tbl'') = change (prc-c) css tbl'
                                   newtbl = Map.insert (prc,css) val tbl''
                                         

