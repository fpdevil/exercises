module Writer where

{--|
   Author      : Sampath
   Maintainer  :
   File        : Writer.hs
   Description : A test of Writer Monad
--}
import           Control.Applicative
import           Data.Monoid



isBig :: Int -> (Bool, String)
isBig x = (x > 9, "Compared with a value 9")

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, xlog) f = (y, xlog ++ ylog)
                        where
                        (y, ylog) = f x

-- applyLog with Monoids
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, xlog) f = (y, xlog `mappend` ylog)
                       where
                       (y, ylog) = f x

-- A Small example from LYAH
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "jelly" = ("ice cream", Sum 21)
addDrink "soup"  = ("potatoes", Sum 18)
addDrink _       = ("vodka", Sum 33)

----------------------------------------------------------------------
--
-- The Writer monad depends on two types. a is the type of the data
-- stored into the monad, and w is the type of the additional information
-- (decoration) which is attached to the monad.
--
-- Its main principle is that computations will append previously existing
-- data with data provided by the function called by >>=. This is the role
-- of >>= to take care of appending all decoration data.
--
newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show)

-- constructor for the Writer
writer :: (a, w) -> Writer w a
writer = undefined

-- Functor instance of Writer
instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

-- Applicative instance of Writer
instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    Writer (f, wf) <*> Writer (a, wa) = Writer (f a, wf `mappend` wa)

-- Monad instance of Writer
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, u)) >>= f = Writer (y, u `mappend` v)
                            where
                            Writer (y, v) = f x

-- tell is the way to add some decoration. Using tell in the context of a Monad
-- will create a new writer with a content of () (unit) and the given decoration.
-- It will then be considered during subsequent bind calls.
tell :: (Monoid w) => w -> Writer w ()
tell x = Writer ((), x)

-- clearing the Writer declaration
clear :: (Monoid w) => Writer w ()
clear = Writer ((), mempty)

-- > runWriter $ tell [1] >> tell [2] >> tell [3] >> clear >> tell [4] >> tell [5]
-- ((),[4,5])

-- Write only the result of the computation, forgetting all about the decoration.
compute :: Writer w a -> a
compute = fst . runWriter

-- Get only the log associated with the value
logs :: Writer w a -> w
logs = snd . runWriter

