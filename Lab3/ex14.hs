{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Prelude hiding (filter)
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use and" #-}
main :: IO()

prod [] = 0
prod (x:xs) = foldl (*) x xs

lenght = foldl (\acc _ -> acc + 1) 0

gcdList :: [Int] -> Int
gcdList = foldl gcd 0

andList = foldl (&&) True

reverseNew :: [a] -> [a]
reverseNew = foldl (\acc x -> x : acc) []

filter pred = foldr (\x acc -> if pred x then x : acc else acc) []

forAll pred = foldr (\x acc -> pred x && acc) True

main = do
    print "Exercise 14"
    print (prod [6, 3, 2, 8])
    print (lenght [6, 3, 2, 8])

    print (gcdList [87, 13, 21, 5])
    
    print (andList [True, False, False, True])
    print (andList [False, False, False, True])
    print (andList [True, True, True, True])

    print (reverseNew [6, 3, 2, 8])
    print (filter (>4) [6, 3, 2, 8])
    print (forAll (<4) [6, 3, 2, 8])
