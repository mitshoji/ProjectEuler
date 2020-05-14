import qualified Data.Map as Map


type Key   = Int
type Value = (Integer, Integer)
type Table = Map.Map Key Value


main :: IO ()
main = print $ length $ filter eval $ mapFold fracMap Map.empty [1..1000]


eval :: (Integer, Integer) -> Bool
eval (a,b) = digitlen a > digitlen b
    where
       digitlen x = length $ show x


fracMap :: Key -> Table -> (Value, Table)
fracMap 0 tbl = ((1, 1), tbl)
fracMap k tbl = case Map.lookup k tbl of
                 Just (a,b) -> ((a,b), tbl)
                 Nothing    -> ((an , bn) , newtbl)
                 where
                    ((am, bm),tbl') = fracMap (k-1) tbl
                    an = am + 2*bm 
                    bn = am + bm
                    newtbl = Map.insert k (an,bn) tbl'


mapFold :: (Key -> Table -> (Value, Table)) -> Table -> [Key] -> [Value]
mapFold _ tbl []     = []
mapFold f tbl (k:ks) = val : mapFold f newtbl ks
    where
       (val, newtbl) = f k tbl


