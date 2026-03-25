main :: IO() 

evenNew n =
    (n == 0) || oddNew (n - 1)

oddNew n =
    (n /= 0) && evenNew (n - 1)

main = do
    print "Exercise 5"
    print(evenNew 11)
    print(evenNew 6)
    print(oddNew 1)
    print(oddNew 12)
