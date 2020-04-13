newPrimes :: [Integer]
newPrimes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod`d /= 0) [ x | x <- [2..bound n]]
    where
       bound q = sqrint [1..q] q
       sqrint (r:rs) q | (r * r) <  q = sqrint rs q
                       | (r * r) == q = r
                       | (r * r) >  q = r-1



newPrimes' :: [Integer]
newPrimes' = 2 : filter isPrime' [3,5..]

isPrime' :: Integer -> Bool
isPrime' n = all (\d -> n `mod`d /= 0) [ x | x <- takeWhile (\d -> d*d <= n) newPrimes']
   {- where
       bound q = sqrint [1..q] q
       sqrint (r:rs) q | (r * r) <  q = sqrint rs q
                       | (r * r) == q = r
                       | (r * r) >  q = r-1
    -}
