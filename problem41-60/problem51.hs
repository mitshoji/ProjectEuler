import Data.List

main :: IO ()
main = print $ find (\xs -> length xs == 8) $ findSequence $ dropWhile ( <= 56003) primes


findSequence :: [Integer] -> [[Integer]]
findSequence = concatMap (map (filter isPrime).replaceDigit) 


-- given integer is converted strings with wild-card *, then replace * with desired digit 

replaceDigit :: Integer -> [[Integer]]
replaceDigit n = map (map read) $  map mkcand $ insertwc (show n)
    -- mkcand :: String -> [String]
    where
       mkcand str = filter (\(c:_) -> c /= '0') $ map (replacewc str) "0123456789"
    -- replacewc :: String -> Char -> String
       replacewc [] _                 = []
       replacewc (c:cs) d | c == '*'  = d : replacewc cs d
                          | otherwise = c : replacewc cs d


insertwc :: String -> [String]
insertwc []     = [""]
insertwc [c]    = [[c]] -- we don't need replace last digit in any case.
insertwc (c:cs) = [ x:xs | x <- [c, '*'], xs <- insertwc cs, (x:xs) /= (c:cs)]


primes :: [Integer]
primes = filter isPrime (2:[3,5..])

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = all (\d -> n `mod` d /= 0) $ takeWhile (\d -> d*d <= n) (2:[3,5..])


