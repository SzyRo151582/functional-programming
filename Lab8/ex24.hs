import Prelude hiding (Maybe, Just, Nothing)
main :: IO()

data Maybe a = Nothing | Just a

maybePosition x l = pos 0 l
    where
        pos _ [] = Nothing
        pos n (l:ls)
            | x == l = Just n
            | otherwise = pos (n + 1) ls

maybeDrop n [] = []
maybeDrop n (Nothing:ls) =
    if n == 0 then Nothing:ls
    else maybeDrop (n - 1) ls
maybeDrop n ((Just l):ls) =
    if n == 0 then Just l:ls
    else maybeDrop (n - 1) ls

maybeSum [] = 0
maybeSum (Nothing:ls) = maybeSum ls
maybeSum ((Just l):ls) = l + maybeSum ls

main = do
    print "Exercise 24"
    
    print (maybePosition 4 [1, 2, 3, 4, 5, 6, 7])
    
    print (maybeDrop 2 [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6])
    print (maybeDrop 3 [Nothing, Just 1, Nothing, Just 2, Nothing, Just 3, Nothing, Just 4])
    
    print (maybeSum [Just 12, Just 3, Just 18, Just 21, Just 15])
    print (maybeSum [Nothing, Nothing, Just 18, Just 5, Just 6, Just 7, Just 30, Nothing])
