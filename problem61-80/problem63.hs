main :: IO ()
main = print $ length 
             $ filter (\(x,m) -> (length $ show x) == m) 
             $ [(n^m,m) | n <-[1..9], m <- [1..100]]

--     m-1 < log_10 n^m < m
-- <=> 1-1/m < log_10 n < 1
-- <=> n <- [1..9], m <- [1..100]
-- since log_10 < 1 & log_10 9 = 0.9542425... < 1 - 1/100
