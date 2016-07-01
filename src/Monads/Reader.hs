module Reader where

{--|
   Author      : Sampath
   Maintainer  :
   File        : Reader.hs
   Description : A test of Reader Monad.
                 Reader   Monad   is   also  called   an   Environment
                 MonadReader.  It represents  a computation, which can
                 read values  from a  shared environment,  pass values
                 from  function  to   another  function,  and  execute
                 sub-computations in  a modified  environment.  Actual
                 Reader Monad is defined in Control.Monad.Reader

                 In  the Immutable  Context,  if a  function wants  to
                 access  some   global  variables,   some  application
                 managers  or  services,  it cannot  pass  context  to
                 function
--}

import           Control.Applicative ()
import           Control.Monad       hiding (guard)

----------------------------------------------------------------------
-- implementation of the guard function
-- guard can interrupt or pass a computational flow
-- based on the truthfulness of a condition. This
-- can be used to implement backtracking.
--
guard :: (Monad m) => Bool -> m ()
guard condition = unless condition $ fail "false condition"
----------------------------------------------------------------------
--
-- Implementation of the Reader Monad
-- Reader Monad allows to retrieve the environment from a configuration
-- for an environment e, and a value v
--
newtype Reader e a = Reader { runReader :: e -> a }

-- Constructor for computations in the reader monad
reader :: (e -> a) -> Reader e a
reader = Reader
-- reader = Reader . id

-- Reader as a Functor
instance Functor (Reader e) where
    fmap f (Reader x) = Reader $ \e -> f (x e)

-- Reader as an Applicative
instance Applicative (Reader e) where
    pure = Reader . const
    Reader f <*> Reader v = Reader (f <*> v)

-- A reader is a monad, which propagates the environment untouched
-- along the computation chain.
instance Monad (Reader e) where
    return = Reader . const
    (Reader x) >>= f = Reader $ \e -> runReader (f (x e)) e

-- Retrieve the value of the environment.
-- if you need access to the environment you ask for it
ask :: Reader e e
ask = Reader id

-- asks lets you retrieve a particular entry if the environment is
-- a list of (key, value) pairs.
asks :: Eq a => a -> Reader [(a, b)] (Maybe b)
asks x = do
     env <- ask
     return $ lookup x env

-- local transforms the Environment
-- Executes a computation in a modified environment.
local :: (e -> b) -> Reader b a -> Reader e a
local f x = do
      env <- ask
      return $ runReader x (f env)
