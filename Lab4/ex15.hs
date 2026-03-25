main :: IO()

insertionsort :: (a -> a -> Bool) -> [a] -> [a]
insertionsort l = foldr (insert l) []
    where
        insert :: (a -> a -> Bool) -> a -> [a] -> [a]
        insert _ x [] = [x]
        insert l x (y:ys)
            | l x y = x : y : ys
            | otherwise = y : insert l x ys

main = do
    print "Exercise 15"
    print (insertionsort (<) [3,1,4,2])
    print (insertionsort (>) [3,1,4,2])
