
main :: IO ()
main = print $ length $ filter evalSides [12,14..1500000]

evalSides :: Int -> Bool
evalSides = (== 1).length.findZs 

findZs :: Int -> [Int]
findZs l = filter (evalWith l) [lwz..upz]
  where
    lwz = ceiling $ (fromIntegral l)/(1 + sqrt 2)
    upz = l `div` 2 - 1

isSqr :: Int -> Bool
isSqr q = q == (floor $ sqrt $ fromIntegral q)^2


evalWith :: Int -> Int -> Bool
evalWith l z = isSqr (2*z^2 -(l-z)^2)
