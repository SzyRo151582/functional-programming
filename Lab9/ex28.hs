import Data.List (unfoldr)
main :: IO()

copy n a = unfoldr step n
    where
        step 0 = Nothing
        step k = Just (a, k - 1)

range n m = unfoldr step n
    where
        step k
            |   k > m = Nothing
            |   otherwise = Just (k, k + 1)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfoldr step
    where
        step [] = Nothing
        step (x:xs) = Just (f x, xs)

data BinTree a = Nil | Node a (BinTree a) (BinTree a)

myTree = Node 5 (Node 3 Nil Nil) (Node 2 (Node 6 Nil Nil) Nil)

main = do
    print "Zadanie 28"
    print (copy 5 'x')
    print (range 3 7)
    print (range 5 3)
    print (mapUnfold (+1) [1,2,3])
