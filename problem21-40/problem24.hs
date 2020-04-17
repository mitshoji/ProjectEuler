import Data.List

main :: IO ()
main = print $ (sort $ permutations "0123456789") !! (1000000 - 1)
