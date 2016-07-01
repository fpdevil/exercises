-----------------------------------------------------------------------------
-- |
-- Module      :  Stack
-- Copyright   :  
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  
-- Stability   :  internal
-- Portability :  
--
-- Basic Stack data structure.
--
-----------------------------------------------------------------------------
module Stack (
Stack,
emptyStack,
isEmpty,
list2stack,
stack2list,
push,
pop,
top
)
where

newtype Stack a = Stack [a]

instance (Show a) => Show (Stack a) where
    show (Stack []) = "Stack[]"
    show (Stack xs) = "Stack" ++ show xs

-- empty stack
emptyStack :: Stack a
emptyStack = Stack []

-- convert a list to stack
list2stack :: [a] -> Stack a
list2stack [] = emptyStack
list2stack xs = Stack xs

-- check if a stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _)  = False

-- convert a stack to list
stack2list :: Stack a -> [a]
stack2list xs
    | isEmpty xs = []
    | otherwise = x
      where Stack x = xs

-- push an item into the stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

-- pop an item from the stack
pop :: Stack a -> (a, Stack a)
pop (Stack []) = error "Cannot POP from an empty stack"
pop (Stack (x : xs)) = (x, Stack xs)

-- peek an item from the stack without removing
top :: Stack a -> a
top (Stack []) = error "Cannot TOP from an empty stack"
top (Stack (x : _)) = x

