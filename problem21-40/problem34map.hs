import qualified Data.Map as Map

type Key = Int
type Value = Int
type Table = Map.Map Key Value

main :: IO ()
main = print $ foldl (\ y (x,_) -> x + y) 0 
     $ filter (\(k,v) -> k==v) 
     $ mapFold factsum (Map.fromList [(0,1)]) [3..999999]


mapFold :: (Key -> Table -> (Value, Table)) -> Table -> [Int] -> [(Key, Value)]
mapFold f tbl [] = []
mapFold f tbl (n:ns) = (n, fst (f n tbl)) : mapFold f newtbl ns
                     where
                        newtbl = snd $ f n tbl


factsum :: Key -> Table -> (Value, Table)
factsum k tbl = case Map.lookup k tbl of
              Just v  -> (v, tbl)
              Nothing -> (val, newtbl)
               where
                  (val' ,tbl') = case (k-1) `mod` 10 of
                                 9 -> factsum (k `div` 10) tbl
                                 otherwise -> factsum (k-1) tbl
                  val = case (k-1) `mod` 10 of 
                        0 -> val' 
                        9 -> val' + 1
                        otherwise -> val' + km * (fst $ factsum km tbl)
                            where
                               km = (k-1) `mod` 10
                  newtbl = Map.insert k val tbl'

