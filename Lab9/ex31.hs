main :: IO()

data BoolExpr = Value Bool
                | And BoolExpr BoolExpr
                | Not BoolExpr

eval (Value x) = x
eval (And x y) = eval x && eval y
eval (Not x) = not (eval x)

aRepr = And (And (Not (Value False)) (Value True)) (Value True)

foldBoolExpr val and not exrp =
    case exrp of
        Value x -> val x
        And x y -> and (foldBoolExpr val and not x) (foldBoolExpr val and not y)
        Not x -> not (foldBoolExpr val and not x)

foldEval = foldBoolExpr id (&&) not

main = do
    print "Exercise 31"
    print (eval aRepr)
