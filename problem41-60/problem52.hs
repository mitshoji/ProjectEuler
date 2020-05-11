import Data.List

main :: IO ()
main = print $ find isPermutations $ map multiples [1..]

multiples :: Int -> [Int]
multiples n = take 6 $ iterate (+ n) n


isPermutations :: [Int] -> Bool
isPermutations (n:ns) = all (\r -> (sort $ show n) == r) $ map (sort.show) ns
