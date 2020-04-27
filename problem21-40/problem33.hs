type Pair = (Int, Int)
type Frac = (Pair, Pair)

main :: IO ()
main = print $ snd $ foldr1 prod $ map lowestTerm $ filter isProper candidates

nums :: [Int]
nums = [1..9]

candidates :: [Frac]
candidates = [((a,b),(b,c)) | a <- nums, b <- nums, c <- nums, a < b, a < c]
          ++ [((b,a),(c,b)) | a <- nums, b <- nums, c <- nums, b < c, a < b]

isProper :: Frac -> Bool
isProper ((a,b),(x,y)) | a == y    = (10*a + b)*x == (10*x + y)*b
                       | b == x    = (10*a + b)*y == (10*x + y)*a
                       | otherwise = False

getValue :: Pair -> Int
getValue p = 10*(fst p) + snd p

lowestTerm :: Frac -> Pair
lowestTerm f = (p `div` d, q `div` d)
             where
                d = gcd p q
                p = getValue (fst f)
                q = getValue (snd f)


prod :: Pair -> Pair -> Pair
prod (a,b) (x,y) = lwt (a*x, b*y)
                 where
                    lwt (p,q) = (p `div` d, q `div` d)
                           where d = gcd p q
