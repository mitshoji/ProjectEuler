import Data.Char

main :: IO ()
main = wordList >>= print. length. (filter $ search triangle). (map getScore). getWords


triangle :: [Int]
triangle = [ t | n <- [1..], let t = n*(n+1) `div` 2]

search :: [Int] -> Int -> Bool
search ns x = elem x $ takeWhile ( <= x) ns

getScore :: String -> Int
getScore = foldl (\a c -> a + score c) 0 
    where score c = ord c - ord 'A' + 1

------- read and arrange the textfile to the list of words :: [String]

wordList :: IO [String]
wordList = fmap words $ readFile "p042_words.txt"

getWords :: [String] -> [String]
getWords str = concatMap words $ map arrangeList str

arrangeList :: String -> String
arrangeList [] = []
arrangeList (c:cs) | c == '\"' = arrangeList cs -- delete '\'
                   | c == ','  = (' ') : arrangeList cs -- replace ',' with space
                   | otherwise = c:arrangeList cs
