main :: IO()

smaller x y = x < y

greater x y = x > y

equal x y = x == y

smallerEqual x y = x <= y 

greaterEqual x y = x >= y

notEqual x y = x /= y

main = do
    print "Exercise 3"
    print (smaller 8 3)
    print (greater 2 11)
    print (equal 3 3)
    print (smallerEqual 14 18)
    print (greaterEqual 20 10)
    print (notEqual 4 4)
