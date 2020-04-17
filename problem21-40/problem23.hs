import Euler
import Data.List

main :: IO ()
main = print $ (sum [1..28123]) - (sum $ mksums abundan)


mksums :: [Int] -> [Int]
mksums xs = nodups
          $ takeWhile (<= 28123) 
          $ sort 
          $ concatMap (zipWith (+) xs) (tails xs)
          where
             nodups [x]                  = [x]
             nodups (x:xs:xss) | x == xs = nodups (xs:xss)
                               | x /= xs = x : nodups (xs:xss)


abundan :: [Int]
abundan = [ x | x <- [1..28123], x < divsSum x]
