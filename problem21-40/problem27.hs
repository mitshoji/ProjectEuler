import Euler

main :: IO ()
main = print $ snd $ maximum nums


nums :: [(Int, Int)]
nums = [(len a b, a * b) | b <- pms,
                           a <- [-999..999],
                           a `mod` 2 /= 0  ]
     where
        len a b = length $ formula a b


formula :: Int -> Int -> [Int]
formula a b = takeWhile isPrime [n*n + a*n + b |n <- [0..b]]


pms :: [Int]
pms = takeWhile (< 1000) primes
