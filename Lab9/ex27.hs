main :: IO()

data BinTree a = Leaf a | Node a (BinTree a) (BinTree a)

-- Path type for binary tree
type TreePath = String

tree = Node 3 (Node 4 (Leaf 7)(Leaf 9))(Leaf 1)
myTree = Node 5 (Node 3 (Leaf 4)(Leaf 9))(Node 2 (Node 6 (Leaf 7)(Leaf 1))(Leaf 8))

value :: TreePath -> BinTree a -> Maybe a
value "" (Leaf a) = Just a
value "" (Node a b c) = Just a
value ('l':xs) (Node a b c) = value xs b
value ('r':xs) (Node a b c) = value xs c
value _ _ = Nothing

search :: Eq a => a -> BinTree a -> Maybe TreePath
search x (Leaf a)
    | x == a = Just ""
    | otherwise = Nothing
search x (Node a b c)
    | x == a = Just ""
    | otherwise =
        case search x b of
            Just y -> Just ('l' : y)
            Nothing -> 
                case search x c of
                    Just y -> Just ('r' : y)
                    Nothing -> Nothing

main = do
    print "Exercise 27"
    print (value "ll" tree)
    print (value "rlr" myTree)
    print (search 4 tree)
    print (search 3 tree)
    print (search 8 myTree)
