import Euler
import Data.List

main :: IO ()
main = print $ (sum [1..28123]) - (sum $ mksums abundant)


mksums :: [Int] -> [Int]
mksums xs = nub
          $ takeWhile (<= 28123) 
          $ sort 
          $ concatMap (zipWith (+) xs) (tails xs)


abundant :: [Int]
abundant = [ x | x <- [1..28123], x < divsSum x]
