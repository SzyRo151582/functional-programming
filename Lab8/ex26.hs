{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
main :: IO()

newtype Set a = Set [a]
    deriving (Show, Eq)

member x (Set xs) = x `elem` xs

subset (Set xs) setB = all (`member` setB) xs

union (Set xs) (Set ys) = Set (foldr addIfMissign ys xs)
    where
        addIfMissign x acc
            | x `elem` acc = acc
            | otherwise = x : acc

intersection (Set xs) setB = Set [x | x <- xs, member x setB]

delete x (Set xs) = Set (filter (/= x) xs)

set1 = Set [1, 2, 3]
set2 = Set [4, 5, 6]

main = do
    print "Exercise 26"
    print (member 2 set1)
    print (member 3 set2)
    print (subset (Set [1, 2]) set1)
    print (subset (Set [4, 5]) set2)
    print (union set1 set2)
    print (intersection set1 set2)
    print (delete 1 set1)
    print (delete 6 set2)
