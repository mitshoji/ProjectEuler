{-
Special Pythagorean triplet
   
Problem 9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

type Pyth = (Int, Int, Int)

findPyth' :: [Pyth]
findPyth' = [(a,b,c) | a <- [1..1000],
                       b <- [a..1000],
                       c <- [b..1000],
                       a^2 + b^2 == c^2,
                       a + b + c == 1000]

-- findPyth' は計算が終わらないので改善する

findPyth :: [Pyth]
findPyth = [(2*u*v, u^2 - v^2, u^2 + v^2) | v <- [1..31],
                                            u <- [v..31],
                                            u^2 + u*v == 500]

-- ピタゴラス数は整数(u,v)を用いて(2*u*v, u^2 - v^2, u^2 + v^2)
-- と表されることを利用

prod :: Pyth -> Int
prod (x,y,z) = x * y * z



main :: IO ()
main = print $ prod $ head findPyth

