collatz :: Int -> [Int]
collatz 1 = []
collatz n = (col n) : collatz (col n)
    where col n | n `mod` 2 == 0 = n `div` 2
                | otherwise      = 3 * n + 1


colSeq :: [(Int, Int)]
colSeq = map (\n -> ((len n), n) ) [1..1000000] 
    where len n = length $ collatz n


main :: IO ()
main = print $ snd $ maximum colSeq
