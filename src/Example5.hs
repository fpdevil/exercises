{-# LANGUAGE MultiWayIf #-}
module Example5 where

{-|
   Author      : Sampath
   Maintainer  :
   File        : Example5.hs

-}

import qualified Data.List as L
import           Prelude   hiding (foldl, foldr)

----------------------------------------------------------------------
-- left fold implementation
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []       = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

-- right fold implementation
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []       = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

-- visualizing the foldl and foldr
showFoldl :: Int -> String
showFoldl n = foldl (\x y -> concat ["( ",x," + ",y," )"]) "0" $ map show [1 .. n]

showFoldr :: Int -> String
showFoldr n = foldr (\x y -> concat ["( ",x," + ",y," ) "]) "0" $ map show [1 .. n]

-- test runs
-- λ> showFoldl 10
-- "( ( ( ( ( ( ( ( ( ( 0 + 1 ) + 2 ) + 3 ) + 4 ) + 5 ) + 6 ) + 7 ) + 8 ) + 9 ) + 10 )"
-- λ> showFoldr 10
-- "( 1 + ( 2 + ( 3 + ( 4 + ( 5 + ( 6 + ( 7 + ( 8 + ( 9 + ( 10 + 0 ) ) ) ) ) ) ) ) ) )"
----------------------------------------------------------------------

-- swapping elements in a list of pairs
swapelems :: [(a, b)] -> [(b, a)]
swapelems = map $ uncurry . flip $ (,)


-- a list of all rational number pairs
-- λ> showRats 6
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1)]
showRats :: (Integral a) => Int -> [(a, a)]
showRats n = take n rats

-- infinite list of rational numbers
rats :: (Integral a) => [(a, a)]
rats = (1, 1) : map next rats
       where
       next (n, 1) = (1, n + 1)
       next (n, m) = (n + 1, m - 1)
----------------------------------------------------------------------

-- Ackermann function
-- This version is very inefficient as it grows very fast
-- λ> ackermann 1 2
-- 4
ackermann :: Int -> Int -> Int
ackermann 0 y = y + 1
ackermann x 0 = ackermann (x - 1) 1
ackermann x y = ackermann (x - 1) (ackermann x (y - 1))
----------------------------------------------------------------------

interleave :: a -> [a] -> [[a]]
interleave x []          = [[x]]
interleave x xs@(y : ys) = (x : xs) : map (y :) (interleave x ys)

-- λ> interleave 's' ['a','b','c']
-- ["sabc","asbc","absc","abcs"]

flatten :: [[a]] -> [a]
flatten xs = iter xs []
        where
        iter [] acc       = reverse acc
        iter (y : ys) acc = iter ys (aux y acc)

-- helper function aux for the above
aux :: [a] -> [a] -> [a]
aux xs ys = foldl (flip (:)) ys xs
-- aux [] ys       = ys
-- aux (x : xs) ys = aux xs (x : ys)

flatmap :: (a -> [b]) -> [a] -> [b]
flatmap f xs = flatten (map f xs)

powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset [x]      = [[], [x]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

-- λ> powerset ['a','b','c']
-- ["","c","b","bc","a","ac","ab","abc"]

delete :: (Eq a) => a -> [a] -> [a]
delete _ [] = []
delete x (y : ys)
       | x == y    = ys
       | otherwise = y : delete x ys

permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x : ys | x <- xs, ys <- permutations (delete x xs)]
----------------------------------------------------------------------
-- packing numbers in sequence
data PackInts = Int Integer | Pack Integer Integer deriving Show

packlist :: [Integer] -> [PackInts]
packlist [] = error "empty list"
packlist (x : xs) = fun xs [Int x]
                    where
                    fun [] acc = reverse acc
                    fun (y : ys) acc@(Int z : zs)
                        | y - z == 1 = fun ys (Pack z y : zs)
                        | otherwise  = fun ys (Int z : acc)
                    fun (y : ys) acc@(Pack str en : zs)
                        | y - en == 1 = fun ys (Pack str y : zs)
                        | otherwise   = fun ys (Int y : acc)


-- λ> packlist [1,2,3,4,5,8,9,10,14,15,20,21,22]
-- [Pack 1 5,Pack 8 10,Pack 14 15,Pack 20 22]

-- check if a number divides another number
divides :: (Integral a) => a -> a -> Bool
divides d n
  | d == 0     = error "Division By Zero"
  | otherwise = n `rem` d == 0

-- list of all positive divisors of an integer
divisors :: Int -> [Int]
divisors n = filter (`divides` n) [1 .. abs n]

-- λ> divisors 102
-- [1,2,3,6,17,34,51,102]

-- least divisor function ldf
-- the ldf(k, n) function returns the least divisor
-- of n starting from a value which is ≥ k
--
ldf :: (Integral a) => a -> a -> a
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

-- find out the least prime divisor of a number n by
-- checking all values of x within the range 2 ≤ x ≤ √n
ld :: (Integral a) => a -> a
ld = ldf 2

-- primality test
primality :: (Integral a) => a -> Bool
primality x | x < 1     = error "Not a Positive Integer"
            | x == 1    = False
            | otherwise = ld x == x

-- list primes till the limit
showPrimes :: (Integral a) => Int -> [a]
showPrimes x = take x (filter primality [1..])

-- map all the 26 lower case alphabets with the first 26 primes
alphaPrimes :: [(Char, String)]
alphaPrimes = zip ['a'..'z'] (map show $ showPrimes 26)

-- λ> alphaPrimes
-- [('a',"2"),('b',"3"),('c',"5"),('d',"7"),('e',"11"),
--  ('f',"13"),('g',"17"),('h',"19"),('i',"23"),('j',"29"),
--  ('k',"31"),('l',"37"),('m',"41"),('n',"43"),('o',"47"),
--  ('p',"53"),('q',"59"),('r',"61"),('s',"67"),('t',"71"),
--  ('u',"73"),('v',"79"),('w',"83"),('x',"89"),('y',"97"),
--  ('z',"101")]

-- prime factors of a number
factors :: (Integral a) => a -> [a]
factors x | x < 1     = error "Negative Integer"
          | x == 1    = []
          | otherwise = f : factors (x `div` f)
                        where
                        f = ld x

-- λ> factors 182
-- [2,7,13]
-- λ> factors 268
-- [2,2,67]

-- factorization. Group and list the prime factors of a number
factorization :: (Integral a) => a -> [(a, Int)]
factorization x = map (\c -> (head c, length c)) $ L.group (factors x)

-- λ> factorization 268
-- [(2,2),(67,1)]
-- λ> factorization 1234567890
-- [(2,1),(3,2),(5,1),(3607,1),(3803,1)]

----------------------------------------------------------------------
{-
        Integer Partition

        Partitions are monotonously decreasing sequences of positive
        integers. As per the Ferrer's diagram a partition like [6, 3, 1]
        can be represented as follows
        Partition [6, 3, 1]

        [][][][][][]
        [][][]
        []



-}
----------------------------------------------------------------------

-- data type and type class definitions
-- define a type definition for Partition
newtype Partition = Partition [Int] deriving (Eq, Ord, Show, Read)

-- define Emptyness
class CanBeEmpty a where
      isEmpty :: a -> Bool
      empty   :: a

-- define Partitions. class for number of parts
class HasNumParts a where
      numParts :: a -> Int

class HasWidth a where
      width :: a -> Int

class HasHeight a where
      height :: a -> Int

class HasWeight a where
      weight :: a -> Int
----------------------------------------------------------------------

mkPartition :: [Int] -> Partition
mkPartition xs = Partition $ L.sort $ filter (> 0) xs

-- check if the input is a non-decreasing sequence of
-- positive integers and return True. Otherwise False
isPartition :: [Int] -> Bool
isPartition [] = True
isPartition [x] = x > 0
isPartition (x : y : ys) = (x >= y) && isPartition (y : ys)

isEmptyPartition :: Partition -> Bool
isEmptyPartition (Partition p) = null p

emptyPartition :: Partition
emptyPartition = Partition []

instance CanBeEmpty Partition where
         empty   = emptyPartition
         isEmpty = isEmptyPartition

-- Get the first element of the sequence
partitionHeight :: Partition -> Int
partitionHeight (Partition p) = case p of
                                  (x : _) -> x
                                  []      -> 0

-- Get the length of the sequence or the number of partitions
partitionWidth :: Partition -> Int
partitionWidth (Partition p) = length p

instance HasHeight Partition where
         height = partitionHeight

instance HasWidth Partition where
         width = partitionWidth

-- a tuple containing height and width
heightWidth :: Partition -> (Int, Int)
heightWidth p = (height p, width p)

-- get the weight of the partition
-- this is the sum of the corresponding sequence
partitionWeight :: Partition -> Int
partitionWeight (Partition p) = sum p

instance HasWeight Partition where
         weight = partitionWeight

-- conversion functions to partition
-- if the input is decresing, then its unsafe
toPartitionUnsafe :: [Int] -> Partition
toPartitionUnsafe = Partition

-- if the input is an integer partition
toPartition :: [Int] -> Partition
toPartition xs = if isPartition xs
                 then toPartitionUnsafe xs
                 else error "toPartition FAIL: not a partition"

-- diffSeq
-- Compute the sequence of differences [a₁ - a₂, a₂ - a₃,...,aₙ - 0]
-- from a sequence of [a₁, a₂, a₃,...,aₙ]
differenceSeq :: [Int] -> [Int]
differenceSeq = loop
                where
                loop (x : y : ys) = (x - y) : loop (y : ys)
                loop [x]          = [x]
                loop []           = []

-- elements of the partition
elements :: Partition -> [(Int, Int)]
elements (Partition p) = helper p
         where
         helper part = [(i, j) | (i, k) <- zip [1 .. ] part, j <- [1 .. k]]

-- parts of a number
parts :: Int -> [[Int]]
parts n
    | n == 0 = [[]]
    | otherwise = [x : rest | x <- [1 .. n], rest <- parts (n - x), [x] >= take 1 rest]

-- partitions of an Integer
partitions :: Int -> [Partition]
partitions = map Partition . parts

-- λ> partitions 3
-- [Partition [1,1,1],Partition [2,1],Partition [3]]
----------------------------------------------------------------------
-- testing type classes
class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
    fromNumber = Age
    toNumber (Age n) = n

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
    fromNumber = Year
    toNumber (Year y) = y

sumNum :: (Numberish a) => a -> a -> a
sumNum a b = fromNumber summed
    where
    numA = toNumber a
    numB = toNumber b
    summed = numA + numB

-- λ> sumNum (Age 4) (Age 5)
-- Age 9
----------------------------------------------------------------------
data DaysOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Enum, Show)

data Date = Date DaysOfWeek Int

instance Eq DaysOfWeek where
    (==) Sun Sun = True
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) _ _     = False

instance Ord DaysOfWeek where
    Sat `compare` Sat = EQ
    Sat `compare` _ = GT
    _ `compare` Sat = LT
    _ `compare` _ = EQ


instance Eq Date where
    (==) (Date day daynum) (Date day' daynum') = day == day' && daynum == daynum'

----------------------------------------------------------------------
-- division with repeated subtraction
-- result is a tuple of (quotient, remainder)
dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy n d = loop n d 0
          where
          loop x y acc
              | x < y     = (acc, x)
              | otherwise = loop (x - y) y (acc + 1)
