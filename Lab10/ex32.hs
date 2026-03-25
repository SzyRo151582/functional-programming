main :: IO()

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)

instance (Show a) => Show (BinTree a) where
    show (Leaf x) = show x
    show (Node l r) = "<" ++ show l ++ "|" ++ show r ++ ">"

instance Eq a => Eq (BinTree a) where
    Leaf x == Leaf y = x == y
    Node l1 r1 == Node l2 r2 =
        l1 == l2 && r1 == r2
    _ == _ = False

instance Ord a => Ord (BinTree a) where
    Leaf a <= Leaf b = a <= b
    Leaf _ <= Node _ _ _ = True
    Node _ _ _ <= Leaf _ = False
    Node a l1 r1 <= Node b l2 r2 =
        a < b || (a == b && (l1 < l2 || (l1 == l2 && r1 <= r2)))

newtype Set a = Set (BinTree a)

myTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
mySet = Set [15, 26, 37, 48, 9]

main = do
    print "Exercise 32"
    print mySet
    print myTree
