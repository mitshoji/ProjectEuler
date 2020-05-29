import Data.Char

main :: IO ()
main = print $ sum. (map digitToInt). show $ (!!) exFracP 99


exFracP :: [Integer]
exFracP = 2 : 3 : zipWith (+) (zipWith (*) (drop 2 expExt) (tail exFracP)) exFracP
-- e = [a_0; a_1, a_2, a_3, ... ,a_n]
-- e = p_n / q_n
-- p_n = a_n * p_(n-1) + p_(n-2) 
-- p_1 = 2, p_2 = 3 (we know this thanks to the problem introduction)


expExt :: [Integer]
expExt = 2:1:2:expExt' 2
    where expExt' n = 1:1:(2*n) : expExt' (n+1)
