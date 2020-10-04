main :: IO ()
main = print $ sum $ map (length.numReader) [1..1000]


numReader :: Int -> String
numReader n =  hReader n ++ toReader n

hReader :: Int -> String
hReader n | n < 100          = ""
          | n == 1000        = "onethousand"
          | n `mod` 100 == 0 = digitReader (n `div` 100)
                            ++ "hundred"
          | otherwise        = digitReader (n `div` 100) 
                            ++ "hundredand"

toReader :: Int -> String
toReader n | mod100 == 10 = "ten"
           | mod100 < 20  = specificReader mod100
           | otherwise    = digitReader (10 * (mod100 `div` 10))
                         ++ digitReader (mod100 `mod` 10)
             where mod100 = n `mod` 100



specificReader :: Int -> String
specificReader m | m < 10    = digitReader m
                 | otherwise = teenReader m

teenReader :: Int -> String
teenReader k | k == 10 = ""
             | k == 11 = "eleven"
             | k == 12 = "twelve"
             | k == 13 = "thirteen"
             | k == 14 = "fourteen"
             | k == 15 = "fifteen"
             | k == 16 = "sixteen"
             | k == 17 = "seventeen"
             | k == 18 = "eighteen"
             | k == 19 = "nineteen"

digitReader :: Int -> String
digitReader d | d == 0 = ""
              | d == 1 = "one"
              | d == 2 = "two"
              | d == 3 = "three"
              | d == 4 = "four"
              | d == 5 = "five"
              | d == 6 = "six"
              | d == 7 = "seven"
              | d == 8 = "eight"
              | d == 9 = "nine"
              | d == 20 = "twenty"
              | d == 30 = "thirty"
              | d == 40 = "forty"
              | d == 50 = "fifty"
              | d == 60 = "sixty"
              | d == 70 = "seventy"
              | d == 80 = "eighty"
              | d == 90 = "ninety"
