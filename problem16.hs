{-
Power digit sum
   
Problem 16
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?

-}

main :: IO ()
main = print $ sum $ breakToDigits (2^1000)


breakToDigits :: (Integral a, Read a, Show a) => a -> [a] 
breakToDigits n = map (read. return) $ show n


--Answer: 1366
