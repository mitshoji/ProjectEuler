import Euler

main :: IO ()
main = print $ takeWhile (<1000) primes
