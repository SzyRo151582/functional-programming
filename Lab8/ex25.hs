main :: IO()

data BinTree a = Leaf a | Node a (BinTree a) (BinTree a)

tree = Node 3 (Node 4 (Leaf 7)(Leaf 9))(Leaf 1)
myTree = Node 5 (Node 3 (Leaf 4)(Leaf 9))(Node 2 (Node 6 (Leaf 7)(Leaf 1))(Leaf 8))

heightBinTree (Leaf a) = 1
heightBinTree (Node a b c) = 1 + max (heightBinTree b)(heightBinTree c)

sizeBinTree (Leaf a) = 1
sizeBinTree (Node a b c) = 1 + sizeBinTree b + sizeBinTree c

maxBinTree (Leaf a) = a
maxBinTree (Node a b c) = max a (max (maxBinTree b)(maxBinTree c))

postBinTree (Leaf a) = [a]
postBinTree (Node a b c) = postBinTree b ++ postBinTree c ++ [a]

foldBinTree fLeaf fNode (Leaf a) = fLeaf a
foldBinTree fLeaf fNode (Node a b c) =
    fNode a (foldBinTree fLeaf fNode b)
            (foldBinTree fLeaf fNode c)

heightFoldBinTree = foldBinTree (const 1) (\a b c -> 1 + max b c)

sizeFoldBinTree = foldBinTree (const 1) (\a b c -> 1 + b + c)

maxFoldBinTree :: Ord a => BinTree a -> a
maxFoldBinTree = foldBinTree id (\a b c -> max a (max b c))

postFoldBinTree = foldBinTree (:[]) (\a b c -> b ++ c ++ [a])

main = do
    print "Exercise 25"
    
    print (heightBinTree tree)
    print (heightBinTree myTree)
    
    print (sizeBinTree tree)
    print (sizeBinTree myTree)
    
    print (maxBinTree tree)
    print (maxBinTree myTree)
    
    print (postBinTree tree)
    print (postBinTree myTree)

    print (heightFoldBinTree tree)
    print (heightFoldBinTree myTree)
    print (sizeFoldBinTree tree)
    print (sizeFoldBinTree myTree)
    print (maxFoldBinTree tree)
    print (maxFoldBinTree myTree)
    print (postFoldBinTree tree)
    print (postFoldBinTree myTree)
