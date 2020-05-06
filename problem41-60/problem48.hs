main :: IO ()
main = print $ reverse $ take 10 $ reverse $ show getSelfPowSum

getSelfPowSum :: Integer
getSelfPowSum = foldl1 (\n m -> n + m^m) [1..1000]
