import Data.Char

-- We will use the property of Pell's equation

main :: IO ()
main = print $ snd. maximum $ [ ((x,y),d) | d <- numbers, let (x,y) = approx d]


approx :: Integral a => a -> (a,a)
approx n | even $ length exs = (p , q)
         | odd  $ length exs = (p', q')
    where
       exs = sqrExpand n
       p   = exFrac 0 1 exs
       q   = exFrac 1 0 exs
       p'  = p^2 + n * q^2
       q'  = 2 * p * q 


sqrExpand :: Integral a => a -> [a]
sqrExpand n = endPoint $ expand 0 1 n


expand :: Integral a => a -> a -> a -> [a]
expand p q d | (d - p^2) `mod` q /= 0 = expand (p*(abs q)) (q*(abs q)) (d*q*q)
             | otherwise = x' : expand p' q' d
    where
       x' = floor ((fromIntegral p + (sqrt $ fromIntegral d))/ fromIntegral q)
       p' = x' * q - p
       q' = (d - (p')^2) `div` q


endPoint :: Integral a => [a] -> [a]
endPoint (x:xs) = x : takeWhile (\d -> d /= 2*x) xs


numbers :: Integral a => [a]
numbers = [ n | n <- [1..1000], n /= intSqr n]
    where
       intSqr = (^2).floor.sqrt.fromIntegral


exFrac :: Integral a => a -> a -> [a] -> a
exFrac _ y []     = y
exFrac x y (a:as) = exFrac y (x + a*y) as

