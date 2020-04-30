import qualified Data.Map as Map

type Table = Map.Map Integer Integer

main :: IO ()
main = print $ sum $ map snd $ filter (isPalindrome.fst) 
                             $ mapFold binaryMap Map.empty 
                             $ filter isPalindrome [1,3..999999]
-- only odd numbers can be the palindrome in binary expression
-- since even numbers always have zero in the last digit


isPalindrome :: Integer -> Bool
isPalindrome n = chrn == reverse chrn
    where
       chrn = show n

--running through the list with "MAP"
mapFold :: (Integer -> Table -> (Integer, Table)) -> Table -> [Integer] -> [(Integer,Integer)]
mapFold f tbl [] = []
mapFold f tbl (n:ns) = (fst (f n tbl), n) : mapFold f newtbl ns
    where
       newtbl = snd $ f n tbl

--f(n) = f(n-1) + 1 (n is odd), f(n/2) * 10 (n is even)
binaryMap :: Integer -> Table -> (Integer, Table)
binaryMap 1 tbl = (1, tbl)
binaryMap n tbl = case Map.lookup n tbl of
                       Just v   -> (v, tbl)
                       Nothing  -> (val, newtbl)
                           where
                              (val', tbl') = if n `mod` 2 == 0 
                                             then binaryMap (n`div` 2) tbl
                                             else binaryMap (n-1) tbl
                              val = if n `mod` 2 == 0 
                                    then 10 * val'
                                    else val' + 1
                              newtbl = Map.insert n val tbl'
                              
