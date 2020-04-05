{-
Largest product in a grid
   
Problem 11
In the 20 x 20 grid below, four numbers along a diagonal line 
have been marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 x 63 x 78 x 14 = 1788696.

What is the greatest product of four adjacent numbers in the 
same direction (up, down, left, right, or diagonally) in the 
20 x 20 grid?
-}


grids :: IO [[String]]
grids = fmap (map words.lines) $ readFile "problem11_numbers.txt" 


-- 読み込んだテキスト([[String]])を数字列([[Int]])に変換
arrangeGrids :: [[String]] -> [[Int]]
arrangeGrids =  map (map read)


-- タテ・ヨコ・ナナメ　全方位に拾った数字列をリストにして、リストのリストを作る
everyDirection :: [[Int]] -> [[Int]]
everyDirection xss =  xss 
                   ++ (transpose xss)
                   ++ (diagsRight xss)
                   ++ (diagsLeft xss)


-- 4つの数の積を先頭から順に取る
findProd :: [Int] -> [Int]
findProd []         = []
findProd xxs@(x:xs) = product (take 4 xxs) : (findProd xs)
                      

-- everyDirection の各要素（リスト）から4数の積の最大値を求め、
-- トーナメント方式でそれらの最大値を求める 
eval :: [[Int]] -> Int
eval xss = maximum $ map (maximum.findProd) xss


-- grids を対角線で反転。行列の転置操作
transpose :: [[Int]] -> [[Int]]
transpose [xs]     = [ [x] | x <- xs ] -- [x,y,z] -> [[x],[y],[z]]
transpose (xs:xss) = zipWith (:) xs (transpose xss)
-- ややこしいので例；
--    transpose [[x1,x2,x3],[y1,y2,y3],[z1,z2,z3]] 
-- -> zipWith (:) [x1,x2,x3] (transpose [[y1,y2,y3],[z1,z2,z3]])
-- -> zipWith (:) [x1,x2,x3] (zipWith (:) [y1,y2,y3] (transpose [z1,z2,z3]))
-- -> zipWith (:) [x1,x2,x3] (zipWith (:) [y1,y2,y3] [[z1],[z2],[z3]])
-- -> zipWith (:) [x1,x2,x3] [[y1,z1],[y2,z2],[y3,z3]]
-- -> [[x1,y1,z1],[x2,y2,z2],[x3,y3,z3]]


-- 右斜め下に拾える数字列
-- 例）[[x1,x2,x3],[y1,y2,y3],[z1,z2,z3]]
-- -> [[x1,y2,z3],[x2,y3],[x3],[y1,z2],[z1]]
-- ++ 以降のtailは対角線の重複を削除するため
diagsRight :: [[Int]] -> [[Int]]
diagsRight xss = (diagsRight' xss) ++ (tail $ diagsRight'' xss)


-- 左斜め下に拾える数字列
-- gridsの各行をreverseして右斜め下に拾う操作をすればよい
diagsLeft :: [[Int]] -> [[Int]]
diagsLeft = diagsRight.(map reverse)


-- 行列の上三角化
-- 例）[[x1,x2,x3],[y1,y2,y3],[z1,z2,z3]] -> [[x1,x2,x3],[y2,y3],[z3]] 
diag :: [[Int]] -> [[Int]]
diag []       = []
diag (xs:xss) = xs : diag (map tail xss) 


-- 上三角化したgridsの各行の先頭から1つずつ取る
diagsRight' :: [[Int]] -> [[Int]]
diagsRight' [xs] = [xs]
diagsRight' xss  = (map head $ diag xss) : diagsRight' (map tail $ init xss)


-- 行列の下三角化はgridsを転置して上三角化
diagsRight'' :: [[Int]] -> [[Int]]
diagsRight'' = diagsRight'. transpose 




main :: IO()
main = do grids >>= print. eval. everyDirection. arrangeGrids 



-- Answer: 70600674

