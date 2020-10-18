import qualified Data.Map as Map
import Data.Char(digitToInt)

type Table = Map.Map Int Int

main :: IO ()
main = print $ length $ filter (== 60) 
             $ foldWithTable clMap Map.empty [1..1000000]


clMap :: Int -> Table -> (Int, Table)
clMap k tbl = case Map.lookup fk tbl of
              Just v  -> (v, tbl)
              Nothing -> (ln, tbl')
  where
    fk   = digitFactorial k
    ln   = chainLength k
    tbl' = Map.insert fk ln tbl

foldWithTable::(Int -> Table -> (Int, Table)) -> Table -> [Int] -> [Int]
foldWithTable _ _ []       = []
foldWithTable f tbl (n:ns) = fn : foldWithTable f ntbl ns
  where
    fn   = fst $ f n tbl
    ntbl = snd $ f n tbl


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

digitFactorial :: Int -> Int
digitFactorial n = sum $ map (factorial.digitToInt) $ show n


chain :: Int -> Table -> [Int] -> ([Int], Table)
chain k tbl ns = case Map.lookup k tbl of
              Just v  -> (ns, tbl)
              Nothing -> chain fk newtbl (k:ns)
  where
       fk     = digitFactorial k
       newtbl = Map.insert k fk tbl

chainLength :: Int -> Int
chainLength n = length $ fst $ chain n Map.empty []
