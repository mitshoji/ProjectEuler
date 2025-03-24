main = print $ sum $ map (sum.(take 100). openSq') numList

numList :: [[Int]]
numList = map mkList $ filter (\m -> (m `notElem` [x^2 | x<-[1..10] ])) [1..100]

mkList :: Int -> [Int]
mkList n | n < 10 = [n]
         | otherwise = (n `div` 10) : (n `mod` 10) : []


openSq' :: [Int] -> [Int]
openSq' ns = openSq ns []

openSq :: [Int] -> [Int] -> [Int]
openSq [0] _ = []
openSq ns bs = a : openSq ns' bs'
    where a = maximum [x | x <- [0..9], ((bs ++ [x]) `listProd` x) `listGT` ns]
          ns' = (ns `listSub` ((bs ++ [a]) `listProd` a)) ++ [0,0]
          bs' = (bs ++ [0]) `listSum` ([a] `listProd` 2)

--listGT
listGT :: [Int] -> [Int] -> Bool
listGT xs ys | length xs < length ys = True
               | length xs > length ys = False
               | otherwise = (xs < ys)
 

listSum :: [Int] -> [Int] -> [Int]
listSum ns ms =  dropWhile (==0) $ reverse $ listSumR (reverse ns) (reverse ms) 0

listSumR :: [Int] -> [Int] -> Int -> [Int]
listSumR [] [] 1 = [1]
listSumR [] [] 0 = []
listSumR ns [] d = listSumR ns [0] d
listSumR [] ms d = listSumR [0] ms d
listSumR (n:ns) (m:ms) d | (n+m+d) < 10 = (n+m+d) : listSumR ns ms 0 
                         | otherwise = (n+m+d-10) : listSumR ns ms 1
                         
--listProd, x should be in [0..9] 
listProd :: [Int] -> Int -> [Int] 
listProd [] _ = []
listProd _ 0 = []
listProd ys n = foldl1 listSum  $ take n $ repeat ys

--listSub
listSub :: [Int] -> [Int] -> [Int]
listSub ns ms = dropWhile (==0) $ reverse $ listSubR (reverse ns) (reverse ms) 0

listSubR :: [Int] -> [Int] -> Int -> [Int]
listSubR [] [] _ = []
listSubR ns [] d = listSubR ns [0] d
listSubR [] ms d = [0]
listSubR (n:ns) (m:ms) d | (n-m-d) < 0 =  (10+n-m-d) : listSubR ns ms 1
                         | otherwise = (n-m-d) : listSubR ns ms 0
