main :: IO()

main = do
    putStrLn "Exercise 1"
    putStrLn "10"
    print 10
    print (5 + 3 + 4)
    print (9 - 1)
    print (6 / 2)
    print (2 * 3 + 4 * 6)
    print (3 <= 5 && 5 <= 3)
    putStrLn ""
    print "a"

func1 =
    let a = 3
    in a + 1

func2 =
    let a = 1
    in let b = a + 1
    in a + b + a * b

func3 =
    let square n = n * n
    in square 4
    
func4 =
    let fac n = if n == 0 then 1 else n * fac (n-1)
    in fac 5