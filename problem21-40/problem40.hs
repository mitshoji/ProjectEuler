import Data.Char

main :: IO ()
main = print $ foldl (\x y -> x*takeDigit y) 1 [1,10,100,1000,10000,100000,1000000] 

takeDigit :: Int -> Int
takeDigit n = ((map digitToInt) $ concatMap show [1..])  !! (n-1)
