module WriterEx where

{--|
   Author      : Sampath
   Maintainer  :
   File        : WriterEx
   Description : Examples in Writer Monad
--}

import           Control.Monad.Writer
-- import qualified Data.List            as L

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got the number: " ++ show x])

multiply :: Writer [String] Int
multiply = do
    a <- logNumber 5
    b <- logNumber 6
    tell ["Now multiplying the numbers"]
    return (a * b)

-- GCD of 2 numbers with logging
gcdx :: Int -> Int -> Writer [String] Int
gcdx a b
    | b == 0 = do
      tell ["Finished computing with " ++ show a]
      return a
    | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdx b (a `mod` b)

-- λ> mapM_ print $ snd $ runWriter $ gcdx 1286 224
-- "1286 mod 224 = 166"
-- "224 mod 166 = 58"
-- "166 mod 58 = 50"
-- "58 mod 50 = 8"
-- "50 mod 8 = 2"
-- "8 mod 2 = 0"
-- "Finished computing with 2"

--factorial with logging
fact :: Integer -> Writer [String] Integer
fact x
    | x <= 0 = do
      tell ["Computing the factorial of 0"]
      return 1
    | otherwise = do
      let t = x - 1
      tell ["recursing with x: " ++ show t]
      y <- fact t
      tell ["computing fact = " ++ show y]
      let s = x * y
      tell ["multiplied " ++ show x ++ " and " ++ show y]
      return s

-- factorial with Monoid
fact' :: Integer -> Writer (Sum Integer) Integer
fact' 0 = return 1
fact' n = do
    let t = n - 1
    tell $ Sum 1
    m <- fact' t
    let s = t * m
    tell $ Sum 1
    return s

-- Collatz Sequence
collatzSeq :: Integer -> Writer [Integer] Integer
collatzSeq n = do
    m <- collatz n
    if m == 1
       then return 1
       else collatzSeq m

collatz :: Integer -> Writer [Integer] Integer
collatz n
    | n == 1 = do
      tell [n]
      -- tell ["computing with n: " ++ show n]
      return 1
    | even n = do
      tell [n]
      -- tell [show n ++  " is even, computing " ++ show n ++ " div 2"]
      return (n `div` 2)
    | otherwise = do
      tell [n]
      -- tell [show n ++  " is odd, computing (3 * " ++ show n ++ " + 1)"]
      return (3 * n + 1)

--
-- Towers Of Hanoi
--
--       ==        ||        ||
--      ====       ||        ||
--     ======      ||        ||
--    --------  --------  --------
--       A         B         C

data Pole = A | B | C deriving (Show)

-- 3 poles are as follows
-- Source      src
-- Destination target
-- Auxilliary  aux
--
toh :: Pole -> Pole -> Pole -> Int -> Writer [String] Int
toh src aux target n
    | n == 1 = writer (1, [show src ++ " -> " ++ show target])
    | otherwise = do
      -- move the top n-1 pegs from source to auxilliary via target
      x <- toh src target aux (n - 1)
      -- move the nth peg from source to target
      toh src aux target 1
      -- move the (n-1) pegs from auxilliary to target via source
      y <- toh aux src target (n - 1)
      -- Now return the moves completed
      return (1 + x + y)

-- visualizing the moves
showMoves :: (Int, [String]) -> IO ()
showMoves (n, moves) = do
    putStrLn "--------"
    putStrLn $ "Solved the TOH with " ++ show n ++ " moves!!!"
    -- putStrLn $ L.intercalate ", " moves
    mapM_ print moves
    putStrLn " --------"

hanoi :: Int -> IO ()
hanoi n = showMoves $ runWriter (toh A B C n)
