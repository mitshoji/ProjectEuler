import Data.List 

main :: IO ()
main = print $ length $ nub $ (^) <$> [2..100] <*> [2..100]


