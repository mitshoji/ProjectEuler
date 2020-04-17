import Data.Char
import Data.List

main :: IO ()
main =  namestxt >>= print. nameScore. sort. names


nameScore :: [String] -> Int
nameScore str = sum $ zipWith (*) (map nameValue str) [1..]


nameValue :: String -> Int
nameValue = sum. (map charToInt)
          where
           --  charToInt '\"' = 0
             charToInt c = ord c - ord 'A' + 1

----------------------------------------------------------
names :: [String] -> [String]
names str = concatMap words $ map arrangeList str

arrangeList :: String -> String
arrangeList [] = []
arrangeList (c:cs) | c == '\"' = arrangeList cs
                   | c == ','  = (' ') : arrangeList cs
                   | otherwise = c:arrangeList cs


namestxt :: IO [String]
namestxt = fmap words $ readFile "p022_names.txt"
