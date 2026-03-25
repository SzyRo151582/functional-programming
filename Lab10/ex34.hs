{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
main :: IO()

newtype Set a = Set [a]
    deriving (Show, Eq)

instance Functor Set where
    fmap f (Set xs) = Set (map f xs)

instance Foldable Set where
    foldr f acc (Set xs) = foldr f acc xs

mySet = Set [15, 26, 37, 48, 9]

main = do
    print "Exercise 34"

    print (fmap (`mod` 5) mySet)
    print (foldr (+) 0 mySet)

