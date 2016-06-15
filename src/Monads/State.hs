--
--   Author      : Sampath
--   Maintainer  :
--   File        : State.hs
--   Description : State Monad implementation
--
--                 The  concept  of state  refers  to  a function/computation  global  or
--                 non-local state  of memory  at any  given time. When  the output  of a
--                 function/computation  depends  solely on  its  inputs,  we say  it  is
--                 stateless — an example of this is combinatory logic.  Conversely, when
--                 the output depends not only on  the received input but also on history
--                 of previous executions,  we say it is  stateful — this is  the case in
--                 sequential  logic.  Notice  variables  local to  some  scope  are  not
--                 considered state as they die together with their scope and thus cannot
--                 keep history  of previous executions.  So we only consider  global and
--                 non-local variables to be state.
--
--                 A State  is one or  more variables that  are required to  perform some
--                 computation  but  are   not  among  the  arguments   of  the  relevant
--                 function. The State  monad is a built in monad  in Haskell that allows
--                 for chaining of  a state variables (which may  be arbitrarily complex)
--                 through a series of function calls, to simulate stateful code.
--
--
------------------------------------------------------------------------
module State (
state,
get,
put,
gets,
modify,
evalState,
execState,
withState,
mapState
) where

import           Control.Applicative ()

--
-- define State data type
-- The State type enables us to build computations that manipulate (hidden) state
-- s = type of state of the computation
-- a = type of produced result
-- runState is used to unwrap the State a b value to get the actual state processing
-- function, which is then applied to some initial state
--
newtype State s a = State { runState :: s -> (a, s) }
--
--                    input
--                    state
--                      ⇓
--                   -------
--                   |     |
--                   |     | ⇒ result
--                   |     |
--                   -------
--                      ⇓
--                    output
--                    state
--

-- state constructor
state :: (s -> (a, s)) -> State s a
state = State

-- State monad as an instance of a Functor, it lets the state
-- transformation functionality untouched.
instance Functor (State s) where
    fmap f ma = State $ \s ->
                      let (a, t) = runState ma s
                      in (f a, t)

-- State monad as an Applicative Functor
--
--
--                      s
--                      ⇓
--                   -------
--                   |     |
--                   |  af | ⇒ f ------\
--                   |     |            \
--                   -------             \
--                      ⇓                 \
--                      t                  \
--                      ⇓     af <*> ax     ----> f x
--                      t                  /
--                      ⇓                 /
--                   -------             /
--                   |     |            /
--                   |  ax | ⇒ x ------/
--                   |     |
--                   -------
--                      ⇓
--                      u
--
instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    af <*> ax = State $ \s ->
                      let (f, t) = runState af s
                          (x, u) = runState ax t
                      in (f x, u)

-- State as a Monad
--
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    ma >>= f = State $ \s ->
                     let (a, t) = runState ma s
                         mb = f a
                         (b, u) = runState mb t
                     in (b, u)

-- Monadic functions
-- Retrieve or get the state from the internals of the monad
--
get :: State s s
get = State $ \s -> (s, s)

-- Replace the state inside the monad.
--
put :: s -> State s ()
put x = State $ \_ -> ((), x)

-- modify, modifies the state (read/modify/write).
--	  Main> :t modify ((+1) :: Int -> Int)
--	  modify (...) :: (MonadState Int a) => a ()
--
-- This says that modify (+1) acts over any
-- Monad that is a member of the MonadState class,
-- with an Int state.
--
modify :: (s -> s) -> State s ()
modify f = do
    currentState <- get
    put $ f currentState

-- Gets specific component of the state
--
gets :: (s -> a) -> State s a
gets f = do
    x <- get
    return (f x)

--
-- evalState and execState.  Given a State  a b and an initial state, the
-- function evalState will  give back only the result value  of the state
-- processing, whereas execState will give back just the new state.
--
evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)

-- withState f m executes action m on a state modified by applying f.
withState :: (s -> s) -> State s a -> State s a
withState f m = modify f >> m

-- Map both the return value and final state of a computation using the given function.
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m
