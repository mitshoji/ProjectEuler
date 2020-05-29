
main :: IO ()
main = print $ length $ filter odd $ map (length.tail.sqrExpand) numbers

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
endPoint (x:xs) = (x : takeWhile (\d -> d /= 2*x) xs) ++ [2*x] 

numbers :: Integral a => [a]
numbers = [ n | n <- [1..10000], n /= intSqr n]
    where
       intSqr = (^2).floor.sqrt.fromIntegral
