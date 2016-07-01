module Writer where

{-|
   Author      : Sampath
   Maintainer  :
   File        : Writer.hs
   Description : A test of Writer Monad. Simple Writer Monad implementation
-}
import           Control.Applicative ()
import           Data.Monoid

----------------------------------------------------------------------
-- examples without the actual Writer Monad
--
isBig :: Int -> (Bool, String)
isBig x = (x > 9, "Compared with a value 9")

applyLog' :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog' (x, xlog) f = (y, xlog ++ ylog)
                        where
                        (y, ylog) = f x

-- applyLog using Monoids
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
writer = Writer

-- Functor instance of Writer
instance Functor (Writer w) where
    fmap f (Writer (a, w)) = writer (f a, w)

-- Applicative instance of Writer
instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty) -- pure returns a minimal default context
    Writer (f, wf) <*> Writer (a, wa) = Writer (f a, wf `mappend` wa)

-- Monad instance of Writer
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty) -- similar to the pure
    (Writer (x, u)) >>= f = Writer (y, u `mappend` v)
                            where
                            Writer (y, v) = f x

--
-- tell is the way to add some decoration. Using tell in the context of a
-- Monad will  create a  new writer with  a content of  () (unit)  and the
-- given decoration.  It  will then be considered  during subsequent bind
-- calls.
tell :: (Monoid w) => w -> Writer w ()
tell x = Writer ((), x)

----------------------------------------------------------------------
-- Inside a Writer you can't inspect what has been written, until you run
-- (or "unwrap") the  monad, using execWriter or  runWriter. However, you
-- can use  listen to inspect what  some sub-action wrote to  the writer,
-- before the value is appended to the writer state, and you can use pass
-- to modify what is written.
----------------------------------------------------------------------

-- listen x is an  action that executes the action x  and adds its output
-- to the value of the computation. listen listens to a monad acting, and
-- returns what the monad "said".
listen :: (Monoid w) => Writer w a -> Writer w (a, w)
listen x = Writer ((a, w), w)
           where
           (a, w) = runWriter x

-- pass lets you provide a writer transformer which changes internals of
-- the written object.
-- pass x is an action that executes the action x, which returns a value
-- and a function, and returns the value, applying the function to the output.
pass :: (Monoid w) => Writer w (a, w -> w) -> Writer w a
pass x = Writer (a, f w)
         where
         ((a, f), w) = runWriter x


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

-- Extract the output from a writer computation.
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

-- Map both the return value and output of a computation using the given function.
mapWriter :: ((a, w) -> (b, v)) -> Writer w a -> Writer v b
mapWriter f m = Writer $ f (runWriter m)
