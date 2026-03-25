{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

main :: IO()

append_list l m =
    l ++ m

list_member x l =
    x `elem` l

reverse_list l =
    foldl (flip (:)) []

last_element [l] = l
last_element (l:ls) = last_element ls

delete_elements x [] = []
delete_elements x (l:ls)
    | x == l = delete_elements x ls
    | otherwise = l : delete_elements x ls

pairing [] _ = []
pairing _ [] = []
pairing (l1:l1s) (l2:l2s) = (l1, l2) : pairing l1s l2s

split _ [] = ([], [])
split x (y:ys)
    | y < x = (y:less, more)
    | otherwise = (less, y:more)
    where
        (less, more) = split x ys

mapNew _ [] = []
mapNew f (x:xs) =
    f x : mapNew f xs


main = do
    print "Exercise 9"
    print (append_list [1, 2] [3])
    print (list_member 4 [1, 2, 3, 4])
    print (reverse [5, 4, 3, 2, 1])
    print (last_element [2, 4, 6, 8, 10])
    print (delete_elements 2 [1, 2, 3, 2, 4, 2, 5, 2, 2])
    print (pairing [1,2,3] ['a', 'b', 'c'])
    print (split 6 [2, 4, 6, 8, 10])
    print (mapNew (+5) [1, 2, 3, 4])
