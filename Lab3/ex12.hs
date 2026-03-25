import Prelude hiding (product)
main :: IO()

product term next a b =
    if a > b then 1
    else term a * product term next (next a) b

factorial a =
    if a < 2 then 1
    else product id (+1) 2 a 

main = do
    print "Exercise 12"
    print (product id (+1) 2 5)
    
    print (factorial 1)
    print (factorial 2)
    print (factorial 6)
