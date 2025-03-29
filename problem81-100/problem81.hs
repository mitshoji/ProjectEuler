import qualified Data.Map as Map
import Data.Array.IArray
import Data.Array.Unboxed
type Key = (Int,Int)
type Value = Int
type Table = Map.Map Key Value

main :: IO()
main = matrix >>= print. fst. (\m -> cost m (80,80) Map.empty). (mxy 80) 

cost :: UArray (Int,Int) Int -> Key -> Table -> (Value, Table)
cost arr (0,y) tbl = cost arr (1,y-1) tbl
cost arr (x,0) tbl = cost arr (x-1,1) tbl
cost arr (1,1) tbl = (arr ! (1,1), Map.insert (1,1) (arr ! (1,1)) tbl)
cost arr (x,y) tbl = case Map.lookup (x,y) tbl of
                 Just v -> (v,tbl)
                 Nothing -> (val, newtbl)
                     where
                          val = (min sr su) + arr ! (x,y)
                          (su,tbl') = cost arr (x, y-1) tbl
                          (sr,tbl'') = cost arr (x-1,y) tbl'
                          newtbl = Map.insert (x,y) val tbl''

-- IO txt -> UArray
mxy :: Int -> [[Int]] -> UArray (Int,Int) Int
mxy sz mtx = listArray ((1,1),(sz,sz)) (concat mtx)

------------------------ IO --------------------
matrix :: IO [[Int]]
matrix = fmap (map (map readStr). map words.map numbers.lines) $ readFile "0081_matrix.txt"

numbers :: String -> String
numbers [] = []
numbers (c:cs) | c == ',' = (' '):numbers cs
               | otherwise = c: numbers cs 

readStr :: String -> Int
readStr s = read s
