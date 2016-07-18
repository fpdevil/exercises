module Example2 where


{-|
   Author      : Sampath
   Maintainer  :
   File        : Example2.hs

-}

import           Control.Applicative
import           Control.Monad       hiding (ap, filterM, filterM, foldM, liftM,
                                      sequence, unless, when)
import           Control.Monad.Error hiding (filterM, foldM, sequence)
import           Data.Monoid
import           Prelude             hiding (sequence, sequenceA)

--
-- http://codingbat.com/prob/p104090
-- Given  n>=0, create an array  with the pattern  {1, 1, 2, 1,  2, 3,
-- ... 1, 2, 3  .. n} (spaces added to show the  grouping). Note that the
-- length of the array will  be 1 + 2 + 3 ... + n,  which is known to sum
-- to exactly n*(n + 1)/2.

-- seriesUp(3) → [1, 1, 2, 1, 2, 3]
-- seriesUp(4) → [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]
-- seriesUp(2) → [1, 1, 2]
--
seriesUp :: Int -> [Int]
seriesUp n = concatMap oneToN [1 .. n]
             where
             oneToN x = [1 .. x]

-- lazy sequence of factorials
facts :: (Num a, Enum a) => [a]
facts = 1 : zipWith (*) facts [1 ..]

-- single factorial number
fact :: (Enum a, Num a) => Int -> a
fact = (facts !!)

-- lazy sequence of Fibonacci series
fibs :: (Num a, Enum a) => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Fibonacci number from the above series
fib :: (Num a, Enum a) => Int -> a
fib = (fibs !!)

-- Inserttion Sort
--
insertionsort :: (Ord a) => [a] -> [a]
insertionsort []       = []
insertionsort [x]      = [x]
insertionsort (x : xs) = insert x (insertionsort xs)
                      where
                      insert a [] = [a]
                      insert a (b : bs)
                          | a < b     = a : b : bs
                          | otherwise = b : insert a bs

-- Bubble Sort
--
bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort [x] = [x]
bubblesort (x : xs) = (\(t, ts) -> t : bubblesort ts) $ foldl f (x, []) xs
                      where
                      f (a, acc) b = (min a b, max a b : acc)

--
-- Merge Sort
--
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (left xs)) (mergesort (right xs))
               where
               left = take (length xs `div` 2)
               right = drop (length xs `div` 2)


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = if x <= y
                             then x : merge xs (y : ys)
                             else y : merge (x : xs) ys

-- transpose of a matrix
--
transpose1 :: [[a]] -> [[a]]
transpose1 [] = []
transpose1 [x] = [[s] | s <- x]
transpose1 ([] : ys) = transpose1 ys
transpose1 ((x : xs) : ys) = (x : [h | (h : _) <- ys]) : transpose1 (xs : [t | (_ : t) <- ys])

-- another version
transpose2 :: [[a]] -> [[a]]
transpose2 = foldr (zipWith (:)) (repeat [])
-- transpose2 [] = repeat []
-- transpose2 (xs : xss) = zipWith (:) xs (transpose2 xss)

{----------------------------------------------------------------------
- Idemtity as a Functor
----------------------------------------------------------------------}
-- An Identity type functor and monad
newtype Identity i = Identity { runIdentity :: i } deriving (Show)

instance Functor Identity where
         fmap f x = Identity (f (runIdentity x))
         -- fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
         pure = Identity
         Identity f <*> Identity x = Identity (f x)


instance Monad Identity where
         return = Identity
         -- Identity x >>= f = f x
         x >>= f = f (runIdentity x)


sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr ((<*>) . fmap (:)) (pure [])
-- sequenceA [] = pure []
-- sequenceA (x : xs) = fmap (:) x <*> sequenceA xs

sequence :: [IO a] -> IO [a]
sequence [] = return []
sequence (x : xs) = do
         y <- x
         ys <- sequence xs
         return (y : ys)

-- another version of transpose
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' xs = getZipList $ sequenceA $ map ZipList xs

-- ap for lfting
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf mx = do
   f <- mf
   x <- mx
   return (f x)

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) = do
        l <- p x
        m <- filterM p xs
        return (if l then x : m else m)

-- implement a custom foldM function, which is a monadic version of
-- the standard left fold
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a (x : xs) = f a x >>= (\y -> foldM f y xs)

-- conditional execution of Monadic computations
when :: (Monad m) => Bool -> m () -> m ()
when f s = if f then s else return ()

unless :: (Monad m) => Bool -> m () -> m ()
unless f = Example2.when (not f)

-- Lifting is a monadic operation that converts a non-monadic function
-- into an equivalent function that operates on monadic values
liftM :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f x = x >>= \y -> return (f y)

----------------------------------------------------------------------

compose :: [a -> a] -> a -> a
compose []       = id
compose (f : fs) = f . compose fs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' g z xs = compose (map g xs) z

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
                    where
                    vowels = length . filter (`elem` "aeiou")

fun :: [a] -> a -> [[a]]
fun xs x = [x] : [xs]

myf :: (Enum a, Num a) => [a] -> [[a]]
myf = foldM fun []

myfoo :: [[Integer]]
myfoo = ([] `foo` 1) >>= (`foo` 2) >>= (`foo` 3)
      where
      foo = \xs x -> [x] : [xs]


strcomb :: [a] -> [[a]]
strcomb = filterM (const [True, False])

-- λ> strcomb "ABC"
-- [ "ABC" , "AB" , "AC" , "A" , "BC" , "B" , "C" , "" ]

-- example
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "chocolates" = ("cookies and milk", Sum 27)
addDrink "bread"      = ("coffee", Sum 15)
addDrink "beans"      = ("soup", Sum 40)
addDrink _            = ("beer", Sum 50)

----------------------------------------------------------------------
-- Division
data Expr = Val Int | Div Expr Expr deriving (Show)

-- unsafe division
eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

-- sequencing the operations
seqn :: Maybe a -> Maybe b -> Maybe (a, b)
seqn Nothing _ = Nothing
seqn _ Nothing = Nothing
seqn (Just x) (Just y) = Just (x, y)

-- applying
apply            :: (a -> Maybe b) -> Maybe a -> Maybe b
apply _ Nothing  = Nothing
apply f (Just x) = f x

-- Test examples
ok :: Expr
ok = Div (Div (Val 2012) (Val 2)) (Val 12)

err :: Expr
err = Div (Val 2) (Div (Val 1) (Div (Val 2) (Val 3)))

----------------------------------------------------------------------
-- allCombinations returns a list containing the result of
-- folding the binary operator through all combinations
-- of elements of the given lists
-- For example, allCombinations (+) [[0,1],[1,2,3]] would be
-- [0+1,0+2,0+3,1+1,1+2,1+3], or [1,2,3,2,3,4]
-- and allCombinations (*) [[0,1],[1,2],[3,5]] would be
-- [0*1*3,0*1*5,0*2*3,0*2*5,1*1*3,1*1*5,1*2*3,1*2*5], or [0,0,0,0,3,5,6,10]

allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations _ [] = []
allCombinations f (x : xs) = foldl (liftM2 f) x xs
