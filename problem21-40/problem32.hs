import Data.Char
import Data.List

main :: IO ()
main = print $ sum $ nub $ property pansets


pansets :: [(Int, [Int])]
pansets = [(x*y, arrange [x,y,x*y]) | x <- [1..9], y <- [1234..9876]]
       ++ [(x*y, arrange [x,y,x*y]) | x <- [12..98], y <- [123..987]]
        where
           arrange xs = map digitToInt $ concatMap show xs


property :: [(Int, [Int])] -> [Int]
property [] = []
property (x:xs) | isPandigital $ snd x = (fst x) : property xs
                | otherwise            = property xs  


isPandigital :: [Int] -> Bool
isPandigital xs = length xs == 9 && sort xs == [1..9] 

