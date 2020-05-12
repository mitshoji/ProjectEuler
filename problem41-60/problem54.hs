import Data.Char
import Data.List

main :: IO ()
main = getCards >>= print.sum.(map (game.words))

data Hand =   HighCard Values 
            | OnePair Values
            | TwoPairs Values
            | ThreeCards Values
            | Straight Values
            | Flush Values
            | FullHouse Values
            | FourCards Values
            | StraightFlush Values
            | RoyalFlush Values deriving (Eq, Ord, Show)


type Card  = [Char]
type Cards = [Card]
type Values = [Int]

game crd = judgement p1 p2
    where
       (p1,p2) = (sort $ take 5 crd, sort $ drop 5 crd)

judgement :: Cards -> Cards -> Int
judgement crd1 crd2 | evalCards crd1 < evalCards crd2 = 0
                    | otherwise                       = 1

readCard :: Card -> (Int, Char)
readCard [v,s] | v == 'A' = (14,s)
               | v == 'K' = (13,s)
               | v == 'Q' = (12,s)
               | v == 'J' = (11,s)
               | v == 'T' = (10,s)
               | otherwise = (digitToInt v, s)

takeValue :: Card -> Int
takeValue = fst.readCard

takeSuit :: Card -> Char
takeSuit = snd.readCard

evalCards :: Cards -> Hand
evalCards ccs | (royal ccs) && (flush ccs) = RoyalFlush vals
              | (flush ccs) && (straight ccs) = StraightFlush vals
              | four ccs = FourCards vals
              | fullhs ccs = FullHouse vals
              | flush ccs = Flush vals
              | straight ccs = Straight vals
              | three ccs = ThreeCards vals
              | two ccs   = TwoPairs vals
              | one ccs   = OnePair vals
              | otherwise = HighCard vals

    where vals            = sortWithValues $ map takeValue ccs
          royal           = (==[14,11,13,12,10]). (map takeValue)
          flush (c:cs)    = all ((takeSuit c ==). takeSuit ) cs 
          straight (c:cs) = all (== takeValue c) $ zipWith (-) (sort $ map takeValue cs) [1..4]
          four            = (==[1,4]). sort. (map length). group. (map takeValue)
          fullhs          = (==[2,3]). sort. (map length). group. (map takeValue)
          three           = (==[1,1,3]). sort. (map length). group. (map takeValue)
          two             = (==[1,2,2]). sort. (map length). group. (map takeValue)
          one             = (==[1,1,1,2]). sort. (map length). group. (map takeValue)

sortWithValues :: Ord a => [a] -> [a]
sortWithValues xs = concatMap snd $ reverse $ sort $ zip (map length gs) gs
    where gs = group xs

getCards :: IO [String]
getCards = fmap lines $ readFile "p054_poker.txt"
