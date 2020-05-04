
main :: IO ()
main = print $ sum $ map (read. (addDiff "0123456789")) chain

chain :: [String]
chain = foldl mkshift (mult 17) [13,11,7,5,3,2]

-- add the number that has not been used in the solution of chain function
addDiff :: Eq a => [a] -> [a] -> [a]
addDiff [] ys     = ys
addDiff (x:xs) ys | x `elem` ys = addDiff xs ys
                  | otherwise   = addDiff xs (x:ys) 

-- add all numbers from 0 to 9 by using addN function
mkshift :: [String] -> Int -> [String]
mkshift xs d = concatMap (addN xs d) [0..9] 

-- addN : add given n on the head of the list, with deleting elements which contain duplicates
-- addN also evaluate the divisibility with given d
addN :: [String] -> Int -> Int -> [String]
addN ys d n = filter (\q -> ((head3 q) `mod` d == 0 && nodup q) ) $ map (\y -> addHead n y ) ys
--ys = mult 17, for example

head3 :: String -> Int
head3 n = read $ take 3 $ n

-- add a number on the head
addHead :: Int -> String -> String
addHead h y = (show h) ++ y

-- make a list of the multiples of given n
mult :: Int -> [String]
mult n = [ show' t | m <- [1..(1000 `div` n)], let t = m*n, nodup $ show t]
    where
       show' c | c < 100 = '0' : show c
               | otherwise = show c

-- delete the elements that contain duplicated numbers
nodup :: Ord a => [a] -> Bool
nodup []     = True
nodup (x:xs) = (x `notElem` xs) && nodup xs 

