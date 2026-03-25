import Data.Tree (foldTree)
main :: IO()

data Tree a = Node a [Tree a]

sizeTree (Node _ leaves) = 1 + sum (map sizeTree leaves)

sumTree (Node value leaves) = value + sum (map sumTree leaves)

preTree (Node value leaves) = value : concatMap preTree leaves

foldSizeTree = foldTree (\_ results -> 1 + sum results)

foldSumTree = foldTree (\x results -> x + sum results)

foldPreTree = foldTree (\x results -> x : concat results)

myTree = Node 5 [Node 2 [Node 8 [Node 3 [Node 6 []]]]]

main = do
    print "Exercise 29"
    print (sizeTree myTree)
    print (sumTree myTree)
    print (preTree myTree)
