type Year  = Int
type Month = Int
type Date  = Int

main :: IO ()
main = print $ length$ sundayOnFirst $ twentythCentry 
       where 
          sundayOnFirst  =  filter     (\((_,_,d),str) -> (d,str) == (1,"Sun"))
          twentythCentry =  (takeWhile (\((y,m,d),_)   -> (y,m,d) /= (2001,1,1)))
                           .(dropWhile (\((y,m,d),_)   -> (y,m,d) /= (1901,1,1))) 
                           $ zip (callender (1900,1,1)) days



nextDay :: (Year, Month, Date) -> (Year, Month, Date)
nextDay (y   , 12, 31)                                 = (y + 1,     1, 1    )
nextDay (y   ,  m,  d)  | lastdayOf m == d             = (y    , m + 1, 1    )
                        | otherwise                    = (y    ,     m, d + 1)

    where lastdayOf m   | any (m ==) [1,3,5,7,8,10,12] = 31
                        | any (m ==) [4,6,9,11]        = 30
                        | otherwise                    = 28 + leap y

          leap y        | y == 1900                    = 0 
                        | y `mod` 4 == 0               = 1
                        | otherwise                    = 0



callender :: (Year, Month, Date) -> [(Year, Month, Date)] 
callender (y,m,d) = (y,m,d) : (callender $ nextDay (y,m,d))

days :: [String]
days = cycle ["Mon","Tue", "Wed", "Thu","Fri", "Sat", "Sun"]


