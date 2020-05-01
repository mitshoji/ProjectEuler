import Data.List

main :: IO ()
main = print $ maximum $ filter isPandigital $ map calc [1..9999]

calc :: Int -> String
calc k = take 9 $ concatMap (show.(k*)) [1..bound] 
    where
       bound | k < 10    = 9
             | k < 100   = 5
             | k < 1000  = 3
             | k < 10000 = 2

isPandigital :: String -> Bool
isPandigital = (== "123456789").sort
