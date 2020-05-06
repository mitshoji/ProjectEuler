import qualified Data.Map as Map
import Euler
import Data.List

type Table = Map.Map Int [Int]

main :: IO ()
main = print $ check $ choice 4 4 [] $ filter (\x -> (length $ fst x) == 4) getFactor

-- check no dupulications
check :: Eq a => [([a], b)] -> [([a], b)]
check xs | (length $ nub $ concatMap fst xs) == (sum $ map (length.fst) xs) = xs
         | otherwise = error "not found"

-- choose consecutives, dsr as desired length
choice :: Int -> Int -> [([(Int, Int)],Int)] -> [([(Int, Int)],Int)] -> [([(Int, Int)],Int)]
choice _ 0 ts _                                     = ts
choice dsr cnt [] (x:xs)                            = choice dsr (cnt - 1) [x] xs
choice dsr cnt (t:ts) (x:xs) | (snd t) + 1 == snd x = choice dsr (cnt - 1) (x:t:ts) xs
                             | otherwise            = choice dsr dsr [] (x:xs)


-- get a list of factorized
getFactor :: [([(Int, Int)],Int)]
getFactor = mapFold factM Map.empty [1..]

-- factorization
factM :: Int -> Table -> ([Int], Table)
factM 1 tbl = ([], tbl)
factM k tbl = case Map.lookup k tbl of
              Just v    -> (v, tbl)
              Nothing   -> (val, newtbl)
               where
                  di           = head $ divisors k
                  key          = k `div` di
                  (val', tbl') = factM key tbl
                  val          = sort $ di : val'
                  newtbl       = Map.insert k val tbl'

-- function to fold factorization
mapFold :: (Int -> Table -> ([(Int)], Table)) -> Table ->[Int] -> [([(Int, Int)],Int)]
mapFold _ _ []       = []
mapFold f tbl (k:ks) = (fhead,k) : mapFold f newtbl ks
                     where 
                        fhead       = map gcnt $ group $ fst $ f k tbl
                        gcnt (x:xs) = (x, length (x:xs))
                        newtbl      = snd $ f k tbl
