main :: IO()

dc test end divide combine p =
    if test p
    then end p
    else combine
        (map (dc test end divide combine) (divide p))

mergeTwo [] ys = ys
mergeTwo xs [] = xs
mergeTwo (x:xs) (y:ys)
    | x <= y = x : mergeTwo xs (y:ys)
    | otherwise = y : mergeTwo (x:xs) ys

merge [xs, ys] = mergeTwo xs ys

digits n = length (show (abs n))

splitAtPow10 n m = (n `div` 10 ^ m, n `mod` 10 ^ m)

karatsuba x y = dc test end divide combine (x, y)
    where
        test (a, b) = a < 10 || b < 10
        end (a, b) = a * b
        divide (a, b) =
            let n = max (digits a) (digits b)
                m = n `div` 2
                (a1, a0) = splitAtPow10 a m
                (b1, b0) = splitAtPow10 b m
            in [(a0, b0), (a1, b1), (a0 + a1, b0 + b1)]
        combine [z0, z2, z1plus] =
            let n = max (digits x) (digits y)
                m = n `div` 2
                z1 = z1plus - z0 - z2
            in z2 * 10 ^ (2 * m) + z1 * 10 ^ m + z0

main = do
    print "Exercise 17"

    print (karatsuba 1324 5678)
    print (karatsuba 5345734687823 3895796828)

