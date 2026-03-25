{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
main :: IO()

newtype Set a = Set [a] deriving (Show, Eq)

delete x (Set xs) = Set (filter (/= x) xs)

union (Set xs) (Set ys) = Set (foldr addIfMissign ys xs)
    where
        addIfMissign x acc
            | x `elem` acc = acc
            | otherwise = x : acc

member x (Set xs) = x `elem` xs

data Tree a = Node (Set a) [Tree a] deriving Show

deleteTree x (Node set children) = Node (delete x set) (map (deleteTree x) children)

unionTree (Node set []) = set
unionTree (Node set children) = union set (foldl union (Set []) (map unionTree children))

allTree x (Node set []) = member x set
allTree x (Node set children) = foldl (&&) (member x set) (map (allTree x) children)

main = do
    print "Exercise 30"
    let mySet1 = Set [1,2,3,4]
    print mySet1

    let myTree1 = Node mySet1 [Node (Set [1,2,3]) []]
    print myTree1

    let myTree2 = Node mySet1 [Node (Set [1,2,3]) [], Node (Set [2,4,5,12]) [] ]
    print (unionTree myTree2)

    print (allTree 1 myTree2)
    print (allTree 2 myTree2)
