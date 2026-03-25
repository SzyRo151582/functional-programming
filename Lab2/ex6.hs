main :: IO()

roots a b c =
    if d < 0 then error "Delta is lower than 0" else (x, y)
    where
        x = -b + sqrt d / (2 * a)
        y = -b - sqrt d / (2 * a)
        d = b * b - 4 * a * c

main = do
    print "Exercise 6"
    print (roots 4 3 2)
