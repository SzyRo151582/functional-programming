main :: IO()

map2 _ _ [] = []
map2 _ [] _ = []
map2 f (x:xs) (y:ys) =
    f x y : map2 f xs ys

filter2 p [] = []
filter2 p (x:xs)
    | p x = x : filter2 p xs
    | otherwise = filter2 p xs

takeWhile2 p [] = []
takeWhile2 p (x:xs)
    | p x = x : takeWhile2 p xs
    | otherwise = []

groups [] = []
groups (x:xs) = (x : takeWhile (== x) xs) : groups (dropWhile (== x) xs)

main = do
    print "Exercise 10"
    print (map2 (+) [1, 2, 3] [8, 9, 10])
    print (filter2 (>3) [2, 3, 4])
    print (takeWhile2 odd [1, 2, 3, 5, 6, 7])
    print (takeWhile2 (>0) [1, 4, 2, -1, 5])
    print (groups [1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 7, 7, 2, 2])
