import Data.Ratio

main :: IO ()
main = print $ maximum candidates

candidates :: (Integral a) => [Ratio a]
candidates = [ q % p | p <- [l .. u], let q = 3 * p `div` 7 - 1]
  where
    u = upper
    l = lower (2%5) (3%7)

lower :: (Integral a) => Ratio a -> Ratio a -> a
lower l u | (denominator l') < upper = lower l' u
          | otherwise                = denominator l
  where
    l' = (l + u) / 2


upper :: (Integral a) => a
upper = 1000000
