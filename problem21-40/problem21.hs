import Euler

main :: IO ()
main = print $ sum $ filter isAmicable [2..10000]


divsSum :: Int -> Int
divsSum n = (calcd $ factorization n) - n

calcd :: [(Int, Int)] -> Int
calcd = foldr (\x y -> y * f x) 1
      where
      -- f :: (Int, Int) -> Int
         f (p,r) = (p^(r+1) - 1) `div` (p-1)


isAmicable :: Int -> Bool
isAmicable n = (divsSum n) /= n && (divsSum $ divsSum n) == n

----------------------------------------------------------------

divsSum' :: Int -> Int
divsSum' n = sum $ [ d | d <- [1..(n `div` 2)], n `mod` d == 0]



