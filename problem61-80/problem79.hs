import Data.Char(digitToInt)

main :: IO()
main = nums >>= print.(foldl1 (\x y -> 10*x + y)).select.(foldl1 merge).numbers

nums :: IO [[String]]
nums = fmap (map words.lines) $ readFile "0079_keylog.txt"

numbers :: [[String]] -> [[Int]]
numbers = map (concatMap $ map digitToInt)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys
merge (x:xs) (y:ys) | x == y = x : merge (xs) ys
                    | x < y =  x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys


select :: (Eq a) => [a] -> [a]
select [] = []
select (x:xs)  | x `notElem` xs = x : select xs 
               | x == head xs = x : select xs
               | otherwise = select xs


