import Data.Char

--10^x < x * 9^5 <=> x < 6 -- 6digits maximum

main :: IO ()
main = print $ sum findNums


findNums :: [Int]
findNums = filter (\n -> n == fifthPowersSum n) [2..999999]

fifthPowersSum :: Int -> Int
fifthPowersSum n = sum $ map (^5) $ map digitToInt $ show n
