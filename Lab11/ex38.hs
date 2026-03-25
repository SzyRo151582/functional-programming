main :: IO()

primRek g f x 0 = g x
primRek g f x y = f x (y - 1) (primRek g f x (y - 1))

--Example: primal recursive for add
add = primRek id (\x y z -> z + 1)

mu h x = search 0
    where
        search i = 
            case h x i of
                Just 0 -> Just i
                Just _ -> search (i + 1)
                Nothing -> Nothing

-- Searing smallest i, which the result is equal to 0
hExample x i = Just (x - i)

muExample = mu hExample

main = do
    print "Exercise 38"
    print (add 5 4)
    print (muExample 50)
