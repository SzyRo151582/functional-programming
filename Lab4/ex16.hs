main :: IO()

square num = num * num

iter f 0 = id
iter f n = f . iter f (n - 1)

main = do
    print "Exercise 16"
    print (iter (+1) 5 0)
    print (iter square 0 7)
    print (let f = iter square 2 in f 5)
    print (iter square 2 3)
