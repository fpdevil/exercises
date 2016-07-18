module Example3 where

import           Control.Monad
import qualified Data.List     as L


----------------------------------------------------------------------
-- Sheep Cloning Experiment
----------------------------------------------------------------------
-- Sheep have  parents; most  have two, but  cloned sheep  (e.g., Dolly)
-- have  only one,  and the  first  sheep (called  Adam and  Eve in  this
-- example) have no parents (or at  least their parents were not sheep!) So
-- the mother and father functions have to be of type Sheep -> Maybe Sheep
--
data Sheep = Sheep { name   :: String
                   , mother :: Maybe Sheep
                   , father :: Maybe Sheep
                   } deriving Show

-- Computation of Grand parents, Great Grand parents etc requires dealing
-- with the non determinism like Nothing possibility. So, inorder to
-- compute the Maternal Grand Father, we have to do something like
--
maternalGrandFather :: Sheep -> Maybe Sheep
maternalGrandFather sheep = case mother sheep of
                                Nothing -> Nothing
                                (Just x) -> father x

-- Great Grand Parents
-- define a combinator for combining 2 functions to return Maybe
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _ = Nothing
comb (Just x) f = f x

-- using the function comb to build sequences
maternalGrandFatherC :: Sheep -> Maybe Sheep
maternalGrandFatherC sheep = Just sheep `comb` mother `comb` father

fathersMaternalGrandmotherC :: Sheep -> Maybe Sheep
fathersMaternalGrandmotherC sheep = Just sheep `comb` father `comb` mother `comb` mother

mothersPaternalGrandfatherC :: Sheep -> Maybe Sheep
mothersPaternalGrandfatherC sheep = Just sheep `comb` mother `comb` father `comb` father

-- Sheep family tree
adam   :: Sheep
adam   = Sheep "Adam" Nothing Nothing
eve    :: Sheep
eve    = Sheep "Eve" Nothing Nothing
uranus :: Sheep
uranus = Sheep "Uranus" Nothing Nothing
gaea   :: Sheep
gaea   = Sheep "Gaea" Nothing Nothing
kronos :: Sheep
kronos = Sheep "Kronos" (Just gaea) (Just uranus)
holly  :: Sheep
holly  = Sheep "Holly" (Just eve) (Just adam)
roger  :: Sheep
roger  = Sheep "Roger" (Just eve) (Just kronos)
molly  :: Sheep
molly  = Sheep "Molly" (Just holly) (Just roger)
dolly  :: Sheep
dolly  = Sheep "Dolly" (Just molly) Nothing


-- Print Dolly's maternal grand father
-- Writing the functions in terms of Monads
maternalGrandFatherM :: Sheep -> Maybe Sheep
maternalGrandFatherM sheep = mother sheep >>= father
-- maternalGrandFatherM sheep = return sheep >>= mother >>= father

fathersMaternalGrandmotherM :: Sheep -> Maybe Sheep
fathersMaternalGrandmotherM sheep = father sheep >>= mother >>= mother
-- fathersMaternalGrandmotherM sheep = return sheep >>= father >>= mother >>= mother

mothersPaternalGrandfatherM :: Sheep -> Maybe Sheep
mothersPaternalGrandfatherM sheep = mother sheep >>= father >>= father
-- mothersPaternalGrandfatherM sheep = return sheep >>= mother >>= father >>= father

-- find an ancestor by tracing a list of mother/father relationships via foldM
-- traceFamily is a generic function to find an ancestor
traceFamily :: Sheep -> [Sheep -> Maybe Sheep] -> Maybe Sheep
traceFamily sheep list = foldM getParent sheep list
                         where
                         getParent s f = f s

-- we can define complex queries using traceFamily in an easy, clear way
mothersPaternalGrandfatherT :: Sheep -> Maybe Sheep
mothersPaternalGrandfatherT sheep = traceFamily sheep [mother, father, father]

paternalGrandMotherT :: Sheep -> Maybe Sheep
paternalGrandMotherT sheep = traceFamily sheep [father, mother]

paternalGrandFatherT :: Sheep -> Maybe Sheep
paternalGrandFatherT sheep = traceFamily sheep [father, father]

maternalGrandMotherT :: Sheep -> Maybe Sheep
maternalGrandMotherT sheep = traceFamily sheep [mother, mother]

maternalGrandFatherT :: Sheep -> Maybe Sheep
maternalGrandFatherT sheep = traceFamily sheep [mother, father]

-- returns a parent, if one exists
parent :: Sheep -> Maybe Sheep
parent sheep = mother sheep `mplus`  father sheep

-- returns a grandparent, if one exists.
grandparent :: Sheep -> Maybe Sheep
grandparent sheep = (mother sheep >>= parent) `mplus` (father sheep >>= parent)

-- or in another way using mplus

grandparent' :: Sheep -> Maybe Sheep
grandparent' sheep = paternalGrandFatherT sheep `mplus`
                     paternalGrandMotherT sheep `mplus`
                     maternalGrandFatherT sheep `mplus`
                     maternalGrandMotherT sheep


----------------------------------------------------------------------
-- Prime Numbers
-- define a Least Divisor Function
-- Given 2 numbers den and num, the function ldf
-- returns the smallest divisor of num greater than
-- or equal to the den.
--
ldf :: (Num a, Integral a) => a -> a -> a
ldf den num | num `rem` den == 0  = den
            | den^2 > num        = num
            | otherwise          = ldf (den + 1) num

-- define lazy list of natural numbers
naturals :: [Integer]
naturals = [1 ..]

prime :: Integer -> Bool
prime n | n < 1     = error "Negative number"
        | n == 1     = False
        | otherwise = ldf 2 n == n

primes :: [Integer]
primes = filter prime naturals

-- 1001 th prime number
-- λ> primes !! 1001
-- 7933

-- Collatz Sequence
-- To find the Longest Collatz Sequence starting from a number n
--
collatz :: Integer -> Maybe (Integer, Integer)
collatz n | n == 1     = Nothing
          | even n    = Just (n, n `div` 2)
          | otherwise = Just (n, 3*n + 1)

-- collatz sequence
collatzSeq :: Integer -> [Integer]
collatzSeq = L.unfoldr collatz

-- longest collatz sequence
longestCollatzSeq :: Int
longestCollatzSeq = maximum [length $ collatzSeq n | n <- [1000, 999 .. 1]]

collatzInfo :: IO ()
collatzInfo = do
            print $ collatz 15
            print longestCollatzSeq
            print $ L.elemIndex longestCollatzSeq [length $ collatzSeq n | n <- [1000, 999 .. 1]]

----------------------------------------------------------------------
-- Pairs
----------------------------------------------------------------------
newtype Pair = Pair (Int, Int) deriving Eq

instance Num Pair where
    Pair (a, b) + Pair (x, y) = Pair (a + x, b + y)
    Pair (a, b) - Pair (x, y) = Pair (a - x, b - y)
    Pair (a, b) * Pair (x, y) = Pair (a * x, b * y)
    negate (Pair (a, b))      = Pair (negate a, negate b)
    abs (Pair (a, b))         = Pair (abs a, abs b)
    signum (Pair (a, b))      = Pair (signum a, signum b)
    fromInteger 0             = Pair (0, 0)

instance Ord Pair where
    Pair (a, b) > 0 = min a b >= 0 && max a b > 0

instance Show Pair where
    show (Pair (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"

posRange :: Int -> Int -> [Pair]
posRange a b | a < 0 || b < 0  = []
             | otherwise      = tail [Pair (x, y) | x <- [0 .. a], y <- [0 .. b]]

