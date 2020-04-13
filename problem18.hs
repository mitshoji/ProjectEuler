--Maximum path sum I
   
--Problem 18


main :: IO ()
main = nums >>= print. maximum. findMaxSum. numbers


maxSum :: [Int] -> [Int] -> [Int]
maxSum xs [] = xs
maxSum xs yss@(y:ys) = zipWith max (zipWith (+) xs yss) (zipWith (+) xs ys)
{- example;
   maxSum [7,3,4] [9,2,5,1]
 = zipWith max (zipWith (+) [7,3,4] [9,2,5,1]) (zipWith (+) [7,3,4] [2,5,1])
 = zipWith max [7+9, 3+2, 4+5] [7+2, 3+5, 4+1]
 = zipWith max [16,5,9] [9,8,5]
 = [16,8,9]

-}

findMaxSum :: [[Int]] -> [Int]
findMaxSum [] = []
findMaxSum (xs:xss) = maxSum xs (findMaxSum xss)
{- exmaple;
   findMaxSum [[7],[6,9],[8,1,2]]
 = maxSum [7] (findMaxSum [[6,9],[8,1,2]])
 = maxSum [7] (maxSum [6,9] (findMaxSum [8,1,2]))
 = maxSum [7] (maxSum [6,9] (maxSum [8,1,2] (findMaxSum [])))
 = maxSum [7] (maxSum [6,9] (maxSum [8,1,2] []))
 = maxSum [7] (maxSum [6,9] [8,1,2])
 = maxSum [7] [14,11]
 = [21]
-}
---------------------------------------------------------------
nums :: IO [[String]]
nums = fmap (map words.lines) $ readFile "problem18_numbers.txt" 

numbers :: [[String]] -> [[Int]]
numbers = map (map read)
