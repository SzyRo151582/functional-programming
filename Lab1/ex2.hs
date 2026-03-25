main :: IO()

-- gcdNew :: (Integral Integer) => Integer -> Integer -> Integer
gcdNew a b = 
   if b /= 0 then gcdNew b (mod a b)
   else a

-- lcmNew :: Integer -> Integer -> Integer
lcmNew a b =
   div (a * b) (gcdNew a b)

main = do
    putStrLn "Exercise 2"
    print(gcdNew 12 24)
    print(lcmNew 21 49)
