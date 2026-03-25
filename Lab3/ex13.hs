{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
main :: IO()

foldrNew :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2 -- Typ funkcji foldr
foldrNew a b [] = b
foldrNew a b (x:xs) = a x (foldrNew a b xs)

-- recursive foldleft
foldLeft acc f [] = acc
foldLeft acc f (x:xs) = f x (foldLeft acc f xs)

main = do
    print "Exercise 13"
    print (foldrNew (+) 0 [1, 2, 3, 4])
    print (foldrNew (:) [] [1, 2, 3])

    print (foldLeft 0 (+) [2, 4, 6, 8, 10])
