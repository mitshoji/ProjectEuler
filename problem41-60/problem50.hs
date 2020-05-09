type Index = Int
type Value = Integer
type Pset  = (Index, Value)

main :: IO ()
main = print $ searchPseq $ takeWhile (\(x,y) -> y < 1000000) prmSets

searchPseq :: [Pset] -> Pset
searchPseq [p] | isPrimeSet p = p
               | otherwise    = (0,0)
searchPseq ps = maximum [searchPseq left, searchMiddle left right, searchPseq right]
    where
       (left, right) = (take hlf ps, drop hlf ps)
       hlf           = (length ps) `div` 2

searchMiddle :: [Pset] -> [Pset] -> Pset
searchMiddle ls rs = maximum [ eval x y | x <- ls, y <- rs, isPrimeSet (eval x y)]
    where
       eval (ix,vx) (iy,vy) = (iy-ix, vy-vx)


prmSets :: [Pset]
prmSets = zip [1..] $ pSum 0 (2: [3,5..])
    where
       pSum i (n:ns) | isPrime n = (i+n) : pSum (i+n) ns
                     | otherwise = pSum i ns


isPrimeSet :: Pset -> Bool
isPrimeSet (idx, val) = isPrime val
       
isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod` d /= 0) 
          $ takeWhile (\d -> d*d <= n) (2:[3,5..])

----- other solution; almost equivalent efficiency...

search :: Pset
search = maximum [eval x y | x <- (0,0):takeWhile (\(k,v) -> (v < 1000000)) prmSets,
                             y <- (0,0):takeWhile (\(k,v) -> (v < 1000000)) prmSets,
                             snd x < snd y, isPrimeSet (eval x y)]
    where
       eval (ix,vx) (iy,vy) = (iy-ix, vy-vx)
