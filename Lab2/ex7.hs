main :: IO()

fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibAcc n =
    acc n (0, 1)
    where
        acc !n (!a, !b) | n == 0 = a
                        | otherwise = acc (n - 1) (b, a + b)

main = do
    print "Exercise 7"
    print (fib 8)
    print (fibAcc 11)
