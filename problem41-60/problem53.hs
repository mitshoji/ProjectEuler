import qualified Data.Map as Map

type Key = (Int, Int)
type Value = Integer
type Table = Map.Map Key Value

-- use mapFold
main :: IO ()
main = print $ length $ filter ( > 1000000) $ mapFold combMap Map.empty [(n,r) | n <- [1..100], r <- [0..n]]

combMap :: Key -> Table -> (Value, Table)
combMap (_ , 0) tbl          = (1, tbl)
combMap (1 , _) tbl          = (1, tbl)
combMap (n , r) tbl | n == r = (1, tbl) 
                    | otherwise =  case Map.lookup (n, r) tbl of
                                   Just v  -> (v, tbl)
                                   Nothing -> (val, newtbl)
                                    where
                                       (val',tbl')   = combMap ((n-1),(r-1)) tbl
                                       (val'',tbl'') = combMap ((n-1), r) tbl'
                                       val = val' + val''
                                       newtbl = Map.insert (n,r) val tbl''
                                      
 

mapFold :: (Key -> Table -> (Value, Table)) -> Table -> [Key] -> [Value] 
mapFold _ _ []       = []
mapFold f tbl (k:ks) = val : mapFold f newtbl ks
    where
       (val, newtbl) = f k tbl
