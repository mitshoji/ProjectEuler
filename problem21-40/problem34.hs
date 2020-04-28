import qualified Data.Map as Map
import Data.Char

type Key = Int
type Value = Int
type Table = Map.Map Key Value

main :: IO ()
main = print $ sum $ filter eval list

list :: [Int]
list = [ n | n <- [3..(7*fact 9)], n `mod` 10 /= 0]

eval :: Int -> Bool
eval n = n == (sumup $ btg n)
     where sumup = foldr (\n m -> m + fact n) 0 
--eval n = n == factsum n

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

btg :: Int -> [Int]
btg = map digitToInt.show


factsum :: Int -> Int
factsum n = fst $ factM n Map.empty

factM :: Key -> Table -> (Value, Table)
factM 0 tbl = (1, tbl)
factM k tbl = case Map.lookup k tbl of
              Just v  -> (v, tbl)
              Nothing -> (val, newtbl)
               where
                  (val',tbl') = if k < 10 then (0,tbl) else factM k' tbl 
                  k' = k `div` 10
                  val = val' + fact(k `mod` 10)
                  newtbl = Map.insert k val tbl'


