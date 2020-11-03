import Data.List (permutations, sort,nub)

type Group = [Int]
type Ring  = [Group]
type Nodes = [Int]

main :: IO ()
main = print $ maximum 
     $ filter (\s -> length s == 16) 
     $ gengons [1..10]


gengons :: [Int] -> [String] 
gengons ns = nub $ map (getNumExpr.minhead) 
           $ filter evalRing $ map edges 
           $ separate ns


getNumExpr :: Ring -> String
getNumExpr = (concatMap show). concat 


minhead :: Ord a => [a] -> [a]
minhead (x:xs) | all ( > x ) xs = x:xs
               | otherwise      = minhead (xs ++ [x])


separate :: [Int] -> [(Nodes, Nodes)]
separate xs = map (splitAt n) $ permutations xs
  where
    n = length xs `div` 2


edges :: (Nodes , Nodes) -> Ring
edges (exs, ins) = makeRing exs ins (shift ins)
  where
    shift (x:xs) = xs ++ [x]

makeRing :: Nodes -> Nodes -> Nodes -> Ring
makeRing [] _ _               = []
makeRing _ [] _               = []
makeRing _ _ []               = []
makeRing (x:xs) (y:ys) (z:zs) = [x,y,z] : makeRing xs ys zs


evalRing :: Ring -> Bool
evalRing = allsame.(map sum)
  where
    allsame (x:xs) = all (==x) xs
