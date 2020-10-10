import qualified Data.Map as Map
import Data.Ratio
import Data.List (nub)

type Key   = Int
type Value = [Int]
type Table = Map.Map Key Value


main :: IO ()
main = print $ sum $ foldWithTable totientMap Map.empty [2..1000000]


--φ 関数の計算の律速段階は素因数分解なので、合成数の素因数を求める計算
--を連想リストを使って効率化する。
--もし n が d で割り切れるなら、nの素因数はdと n/d の素因数である。
--小さな数から順番に数の素因数を求めつつそれを覚えておいて、後半の大きな
--数の素因数の計算を効率化する。
--例）12の素因数 = 2 と 6の素因数
--　　　　　　　 = 2 と 2 と 3 --> 素因数は [2,3]
--　よってφ (12) = 12 * (1-1/2) * (1 - 1/3)
totientMap :: Key -> Table -> (Int, Table)
totientMap 1 tbl = (1 , tbl)
totientMap k tbl = case Map.lookup k tbl of
                   Just vs   ->  ( phi k vs, tbl)
                   Nothing   ->  ( phi k vals, newtbl)
                     where
                       vals       = if isPrime k 
                                    then [k]
                                    else nub $ mfact : vs'
                       vs'        = tbl Map.! (k `div` mfact) 
                       mfact      = minFactor k
                       newtbl     = Map.insert k vals tbl



foldWithTable :: (Key -> Table -> (Int, Table)) -> Table -> [Key] -> [Int]
foldWithTable f tbl []     = []
foldWithTable f tbl (k:ks) = fx : foldWithTable f tbl' ks
  where
    (fx, tbl') = f k tbl


 

isPrime :: (Integral a) => a -> Bool
isPrime n = all (\d -> n `mod` d /= 0) $ takeWhile (\d -> d^2 <= n) ( 2 : [3,5..])


primeFactors :: (Integral a) => a -> [a]
primeFactors m  = factors m primes
  where
    primes = filter isPrime $ [2..(bnd m)] ++ [m]
    bnd = ceiling. sqrt. fromIntegral
    factors m []     = [] 
    factors m (p:ps) | m `mod` p == 0 = p : factors (m `div` p) ps
                     | otherwise      = factors m ps

minFactor :: (Integral a) => a -> a
minFactor n = head $ filter (\d -> (isPrime d) && ((n `mod` d) == 0)) [2..n]


phi :: (Integral a) => a -> [a] -> a
phi n ps = numerator $ (n % 1) * (foldl (\x y -> x * (1 - 1 % y)) 1 ps)

