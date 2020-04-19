main :: IO ()
main = print $ sum vertexes - 3

vertexes :: [Int]
vertexes = [4*n*n - 6*(n-1) | n <- [1,3..1001]]

--   n^2 + n^2 - (n-1) + n^2 - 2*(n-1) + n^2 - 3*(n-1)
-- = 4*n^2 - 6*(n-1)
