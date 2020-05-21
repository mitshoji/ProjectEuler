import qualified Data.Map as Map
import Data.List

type Key   = String
type Value = [Integer]
type Table = Map.Map Key Value

main :: IO ()
main = print $ sort $ fst $ cubicM cubic Map.empty
    where
       cubic = [n^3 | n <- [1..]]

cubicM :: [Integer] -> Table -> (Value, Table)
cubicM (n:ns) tbl = case Map.lookup ks tbl of
                      Nothing   -> cubicM ns (Map.insert ks [n] tbl)
                      Just vals -> if length (n:vals) == 5
                                   then (n:vals, tbl)
                                   else cubicM ns newtbl
                                   where
                                      val    = n:vals
                                      newtbl = Map.insert ks val tbl
            where
               ks = sort $ show n
