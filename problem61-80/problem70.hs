import Euler (primes)
import Data.List
import Data.Ratio

main :: IO ()
main = print $ snd $ minimum $ map nByPhi
             $ filter (\z -> proper z && sieve z) candidates

nByPhi :: (Integral a) => (a, a) -> (Ratio a, a)
nByPhi (p, q) = (((p * q) % ((p - 1) * (q - 1))), p * q)
-- n/φ (n) の評価を有理数のまま計算するためにData.Ratioをimportして

factors :: (Integral a) => [a]
factors = takeWhile (\p -> p * p < 100000000) primes
-- この探索範囲の妥当性が分からん。。。


candidates :: (Integral a) => [(a, a)]
candidates = [ (p, q) | p <- factors, 
                        q <- factors, 
                        p < q, 
                        p * q < 10000000]
--２つの素数の積の中から答えを探すことも、結果オーライなだけであって妥当性は未確認

sieve :: (Integral a) => (a, a) -> Bool
sieve (m, n) = (m + n - 1) `isMultipleOf` 9
--２つの数 x と y が数字の入れ替えの関係であれば、その差は９の倍数になる
--証明は簡単

isMultipleOf :: (Integral a) => a -> a -> Bool
isMultipleOf n d = n `mod` d == 0

isPermutation :: (Eq a, Ord a) => [a] -> [a] -> Bool
isPermutation xs ys = (sort xs) == (sort ys)

proper :: (Integral a, Show a) => (a, a) -> Bool
proper (p, q) = isPermutation (show num) (show phi)
  where
    num = p * q
    phi = (p - 1) * (q - 1)

-- real	0m0.221s
-- user	0m0.217s
-- sys	0m0.004s

