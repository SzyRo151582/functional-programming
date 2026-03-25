main :: IO() 

expNew b e = 
    round ((b ** (e / 2)) ** 2)

expRec b 0 = 1
expRec b e
    | e > 0 = b * expRec b (e - 1)
    | otherwise = 1 / expRec b (-e)

main = do
    print "Exercise 8"
    print (expNew 3 3)
    print (expRec 4 3)
