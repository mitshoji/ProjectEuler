import Euler (isPrime)
import Data.List (find)
import qualified Data.Map as Map

type Key = (Int,[Int])
type Value = Integer
type Table = Map.Map Key Value


main :: IO ()
main = print $ 
       find (\n -> (fst $ change n (coins n) Map.empty) > 5000)
       [5..]


coins :: Int -> [Int]
coins n = reverse $ filter isPrime [2..n-1]


change :: Int -> [Int] -> Table -> (Value, Table)
change 0 _ tbl             = (1, tbl)
change _ [] tbl            = (0, tbl)
change prc css@(c:cs) tbl
                 | prc < 0 = (0, tbl)
                 |otherwise = case Map.lookup (prc,css) tbl of
                              Just v  -> (v, tbl)
                              Nothing -> (val, newtbl)
                                where
                                   val            = val' + val''
                                   (val', tbl')   = change prc cs tbl
                                   (val'', tbl'') = change (prc-c) css tbl'
                                   newtbl = Map.insert (prc,css) val tbl''
