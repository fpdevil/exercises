{-# LANGUAGE MultiWayIf #-}
module Example1 where

import           Control.Monad      hiding (guard)
import           Data.Bits
import           Data.Char
import           Prelude            hiding (gcd, negate, signum)
import           System.Environment
import           System.Exit
import           System.IO

signum :: (Num a, Num b, Ord b) => b -> a
signum x = if | x < 0 -> -1
              | x == 0 -> 0
              | x > 0 -> 1

-- Rot13 Algorithm
-- there are 26 letters, rotating twice by 13 takes you back to where you started
-- A generalized rotChar function which would rotate by the supplied integer
rotChar :: Int -> Char -> Char
rotChar n c
        | isLower c = rotCase 'a' c
        | isUpper c = rotCase 'A' c
        | otherwise = c
        where
        rotCase :: Char -> Char -> Char
        rotCase base char = chr (ord base + (ord char - ord base + n) `mod` 26)

-- Inorder to rotate the entire String instead of the Char, we have to lift
-- from rotChar :: Char -> Char
-- to   rot :: String -> String
rot :: Int -> String -> String
rot n = map (rotChar n)

-- Suppose we want to implement other ciphers.
-- For example, the Caesar cipher is rot 3, and would be decoded by rot 23 (or rot-3 !).

rot13 :: IO ()
rot13 = do
      input <- getContents
      let output = rot 13 input
      putStr output

rotStdIn :: Int -> IO ()
rotStdIn n = do
         input <- getContents
         let output = rot n input
         putStr output

usage :: IO ()
usage = do
      progname <- getProgName
      hPutStrLn stderr $ "usage: " ++ progname ++ " [n] "
      exitWith $ ExitFailure 255


main :: IO ()
main = do
          args <- getArgs
          case args of
            [] -> rotStdIn 13 -- default of rot 13
            -- [x] -> rotStdIn (read x)
            [x] | x /= "" && all isDigit x -> rotStdIn (read x)
                | otherwise -> usage
            _ -> usage

--
-- A short start with gcd without space/time constraints
-- Approach 1
-- Using the standard math fact that for any two numbers
-- a, b ∈ ℕ ⇔ a = bq + r then gcd(a, b) = gcd(b, r)
--
gcdx :: (Integral a) => a -> a -> a
gcdx a b = f (abs a) (abs b)
           where
           f x 0 = x
           f x y = f y (x `rem` y)

--
-- Approach 2
-- STEIN's ALGORITHM (Binary GCD)
-- This method is using only the Bitwise operations and
-- and does not use the mathematic multiplication or div
-- Its based on the following
-- ∀ a, b ∈ ℕ
-- gcd(a, b) = 2 • gcd(a/2, b/2) if both a and b are Even
-- gcd(a, b) = gcd(a/2, b)       if a is Even and b is Odd
-- gcd(a, b) = gcd(a, b/2)       if a is Odd and b is Even
-- gcd(a, b) = gcd(a - b, b)     if a and b are Odd ∧ a > b
-- gcd(a, b) = gcd(a, b - a)     if a and b are Odd ∧ a < b
--
-- Basically, it covers the following cases
-- Both numbers are Even
-- If one is Odd and other is Even, gcd cannot be Even
-- If both the numbers are Odd
--
-- It uses Recursion with Bit shifting for Multiplication
-- and Bitwise AND for Division and Even/Odd Check
-- Bitwise operation logic:
-- Sum of two bits can be obtained by performing XOR (^) of
-- the two bits. Carry bit can be obtained by performing
-- AND (&) of two bits
--
--
-- define auxilliary functions for Halving and Doubling,
-- Even and Odd checking the integers using bit shifting
-- funtion for doubling with bitshift left
--
double :: (Bits a) => a -> a
double = flip shiftL 1

-- function for halving with bitshift right
halve :: (Bits a) => a -> a
halve = flip shiftR 1

-- function for Even number check
isEven :: (Bits a, Num a) => a -> Bool
isEven x = x .&. 0x1 == 0

-- function for Odd number check
isOdd :: (Bits a, Num a) => a -> Bool
isOdd x = x .&. 0x1 == 1

{----------------------------------------------------------------------
   BITWISE ADDITION
-----------------------------------------------------------------------
--
-- function for adding two numbers
-- The following are the 4 cases covered for Bits
-- 0 + 0 = 0
-- 0 + 1 = 0
-- 1 + 0 = 0
-- 1 + 1 = 0 (and generates a carry)
-- The sum bit of 2 bits is given by the XOR as follows
--
               x   y  SUM  CARRY
               --  -- ---  -----
               0   0   0     0
               0   1   1     0
               1   0   1     0
               1   1   0     1

XOR of same numbers will always return 0

sum = a XOR b
sum handles the cases 0 + 1 and 1 + 0 i.e., all bit
positions that add up to 1


carry = (a AND b) << 0x1

The a AND b covers all the bith positions for 1 + 1 Since the addition
here returns 0,  the carry becomes important and it  has to be shifted
to the next poostion hence the left shift by 1.

With the above cases covered, we can extend the logic of adding 2 bits
for large  numbers. We use the  fact the if 2  integers have different
bits at  the same  positions then  XOR will  return the  SUM of  the 2
integers and there will be no carry.

a + b bitwise
a  =  5  = 0 1 0 1
b  = 10  = 1 0 1 0
          ---------
a XOR b =  1 1 1 1   = 15
          ---------

If there are any common bits then  we can have a combined carry of all
the  common bits  by taking  the bitwise  AND of  the 2  integers. The
algorithm can be as follows

sum = a
    while (b != 0):
          carry = a AND b
          sum = sum XOR b
          b = carry << 0x1

The last  carry thing  is required  because carry  of position  n gets
added to position (n + 1). Hence we shift to left

----------------------------------------------------------------------}
add :: (Bits a, Num a) => a -> a -> a
add x y
    | y == 0 = x
    | otherwise = add (x `xor` y) (carry `shiftL` 0x1)
                  where
                  carry = x .&. y


--
-- function for the negate of a number
-- If x is a number then ~x will give the 1's complement of x
-- and then by adding 1, we get the 2's complement of number.
-- This number will be the negative number of x.
-- (-x) = (~ x) + 1
--
negate :: (Num a, Bits a) => a -> a
negate 0 = 0
negate x = add (complement x) 1

--
-- function for subtracting two numbers
-- we will negate the second number and add
--
sub :: (Bits a, Num a) => a -> a -> a
sub x 0 = x
sub x y = add x (complement y) + 1
-- sub x y = sub (x `xor` y) ((complement x .&. y) `shiftL` 0x1)

-- function for multiplying the numbers
multiply :: (Bits a, Num a) => a -> a -> a
multiply 1 y = y
multiply x 1 = x
multiply x y = multiply (double x) (halve y) +
               if isOdd y
                  then x
                  else 0

-- function for dividing the numbers
divide :: (Bits a, Num a) => a -> a -> a
divide = undefined

-- Define guard
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero


-- Pythagorean Triplets
-- Pythagorean rule predicate
p :: Integer -> Integer -> Integer -> Bool
p a b c = a*a + b*b == c*c

-- calculate the pythagorean triplets with the fact that in a
-- triangle of sides a, b and c the sides are divided as per
-- the rule a < b < c
--
triplets :: [(Integer, Integer, Integer)]
triplets = do
         a <- [1 .. 25]
         b <- [a .. 25]
         c <- [b .. 25]
         guard (p a b c)
         return (a, b, c)

remove :: [a] -> [[a]]
remove [] = [[]]
remove (x : xs) = xs : map (x :) (remove xs)

-- λ> remove [1..3]
-- [[2,3],[1,3],[1,2],[1,2,3]]

-- insert an item  non-deterministically, in any place, of  a list, e.g.,
-- to  insert  99  in [1,2,3,4],  returning  [99,1,2,3,4],  [1,99,2,3,4],
-- [1,2,99,3,4], [1,2,3,99,4], [1,2,3,4,99]]
ndinsert :: a -> [a] -> [[a]]
ndinsert x []         = [[x]]
ndinsert x l@(y : ys) = (x : l) : map (y :) (ndinsert x ys)

-- λ> ndinsert 99 [1,2,3,4]
-- [[99,1,2,3,4],[1,99,2,3,4],[1,2,99,3,4],[1,2,3,99,4],[1,2,3,4,99]]

-- Now, the  permutation problem  itself. The idea  is: detach  the head,
-- find a permutation of the tail; put the detached head somewhere in the
-- permuted list. Non-deterministically
permute :: [a] -> [[a]]
permute []       = [[]]
permute (x : xs) = concatMap (ndinsert x) (permute xs)

-- using foldr
permutef :: [a] -> [[a]]
permutef = foldr (concatMap . ndinsert) [[]]

-- λ> permute "ABC"
-- ["ABC","BAC","BCA","ACB","CAB","CBA"]

-- powerset
powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = ps ++ [x : ys | ys <- ps]
                    where
                    ps = powerset xs


--
-- Partitions of an integer
--
-- This is a more elaborate combinatoric problem. A partition of N, say 7
-- is  a splitting  (or a  set  of all  splittings)  of N  in k  positive
-- integers such that their sum gives N. Order does not count. So, for 7 we
-- shall have, in an intuitive order

-- 7
-- 6 1
-- 5 2
-- 5 1 1
-- 4 3
-- 4 2 1
-- 4 1 1 1
-- 3 3 1
-- 3 2 2
-- 3 2 1 1
-- 3 1 1 1 1
-- 2 1 1 1 1 1
-- 1 1 1 1 1 1 1
--

-- divides
divides :: Integer -> Integer -> Bool
divides d n = if d == 0
                 then error "Division By Zero"
                 else n `rem` d == 0

-- positive divisors of a number
divisors :: Integer -> [Integer]
divisors 0 = []
divisors n = filter (`divides` n) [1 .. abs n]

-- λ> divisors 100
-- [1,2,4,5,10,20,25,50,100]


-- Ugly numbers are numbers whose only prime  factors are 2, 3 or 5.  The
-- sequence 1, 2, 3,  4, 5, 6, 8, 9, 10, 12, 15,  ...  shows the first 11
-- ugly numbers. By  convention, 1 is included.  Write a  program to find
-- and print the 1500'th ugly number.
ugly :: [Integer]
ugly = 1 : merge (map (2*) ugly) (map (3*) ugly) (map (5*) ugly)
       where
       merge xxs@(x : xs) yys@(y : ys) zzs@(z : zs)
           | x < y && x < z = x : merge xs yys zzs
           | y < x && y < z = y : merge xxs ys zzs
           | z < x && z < y = z : merge xxs yys zs
           | x             == y = merge xs yys zzs
           | y             == z = merge xxs ys zzs
           | z             == x = merge xxs yys zs

uglynum :: IO ()
uglynum = putStrLn $ "The 1500'th ugly number is " ++ show (ugly !! 1499) ++ "."

-- λ> uglynum
-- The 1500'th ugly number is 859963392.
-- First 20 ugly numbers
-- λ> 1 : map (ugly !!) [1 .. 20]
-- [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36,40]
-----------------------------------------------------------------------
-- Parenthesis balance check in Haskell
-- Check if an expression containing parenthesis
-- {} [] () is balanced
isBalanced :: String -> Bool
isBalanced c = checkStack c []

checkStack :: String -> String -> Bool
checkStack [] s = null s
checkStack (x : xs) s
    | x == '{' || x == '[' || x == '('  = checkStack xs (x : s)
    | otherwise                    = case s of
                                        []     -> False
                                        z : zs -> ((x == '}' && z == '{') ||
                                                   (x == ']' && z == '[') ||
                                                   (x == ')' && z == '(')) &&
                                                  checkStack xs zs

-- λ> isBalanced "[{}]"
-- True
-- λ> isBalanced "[{}])"
-- False
-- λ> isBalanced "[{(}])"
-- False
-- λ> isBalanced "([{}])"
-- True
-----------------------------------------------------------------------
