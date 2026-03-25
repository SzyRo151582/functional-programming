main :: IO()

-- Example
-- > same-values == equal 3 1
--   True
-- > same-values < > 2 3
--   False
-- > same-values + * 2 2
--   True

sameValues p1 p2 x1 x2 = p1 x1 x2 == p2 x1 x2

main = do
    print "Exercise 4"
    print (sameValues (==) (==) 3 1)
    print (sameValues (<) (>) 2 3)
    print (sameValues (+) (*) 2 2)
