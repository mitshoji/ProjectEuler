import Data.List
import Data.Ratio

main :: IO ()
main = print $ searchNeighbors (3 % 7) subtractors

searchNeighbors :: (Integral a) => Ratio a -> [Ratio a] -> Maybe (Ratio a)
searchNeighbors b []     = Nothing
searchNeighbors b (d:ds) | (denominator (b-d) ) < bnd =  Just (b-d)
                         | otherwise = searchNeighbors b ds

subtractors :: (Integral a) => [Ratio a]
subtractors = [ 1 % d | d <- reverse [(bnd `div` 2) .. bnd]]
    
bnd :: (Integral a) => a
bnd = 1000000


