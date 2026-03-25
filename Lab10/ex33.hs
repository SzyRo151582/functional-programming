import System.Win32 (xBUTTON1)
main :: IO()

class Stack s where
    empty :: s a
    push :: a -> s a -> s a
    pop :: s a -> s a
    top :: s a -> a
    isEmpty :: s a -> Bool 

data MyStack a = Empty | Node a (MyStack a) deriving Show

instance Stack MyStack where
    empty = Empty

    push = Node

    pop Empty = Empty
    pop (Node x xs) = xs

    top Empty = error "Stack is empty"
    top (Node x xs) = x

    isEmpty Empty = True
    isEmpty _ = False

newtype ListStack a = ListStack [a] deriving Show

instance Stack ListStack where
    empty = ListStack []

    push x (ListStack xs) = ListStack (x:xs)

    pop (ListStack []) = ListStack []
    pop (ListStack (x:xs)) = ListStack xs

    top (ListStack []) = error "Stack is empty"
    top (ListStack (x:xs)) = x

    isEmpty ListStack xs = null xs

