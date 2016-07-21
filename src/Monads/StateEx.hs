module StateEx where

{-|
   Author      : Sampath
   Maintainer  :
   File        : StateEx
   Description : Examples using the State Monad
-}

import           Control.Monad.State
import           Prelude             hiding (gcd)

------------------------------------------------------------------------
-- simple examples
inc :: State Int Int
inc = do
    n <- get
    put (n + 1)
    return n

incBy :: Int -> State Int Int
incBy n = do
      n <- get
      modify (+ n)
      return n

testInc :: IO ()
testInc = do
        print $ evalState inc 1
        print $ execState inc 1
        print $ runState inc 1
        print $ runState (withState (+3) inc) 1
        print $ runState (incBy 5) 10
        print $ runState (mapState (\(a, s) -> (a + 4, s + 5)) inc) 1
------------------------------------------------------------------------
-- a factorial function which stores the number of recursive operations
factorial :: Int -> State Int Int
factorial 0 = do
    put 0
    return 1
factorial n = do
    t <- factorial (n - 1)
    modify (+1)
    return (n * t)

-- λ> runState (factorial 10) 0
-- (3628800,10)

-- fibonacci function which stores the number of recursive operations
fibonacci :: Int -> State Int Int
fibonacci 0 = return 1
fibonacci 1 = return 1
fibonacci n = do
    a <- fibonacci (n - 1)
    b <- fibonacci (n - 2)
    modify (+1)
    return (a + b)

-- λ> runState (fibonacci 10) 0
-- (89,88)
------------------------------------------------------------------------
-- Calculator using state

-- declare data type for Calculation State intended for internal use
-- add memory location for the calculaltor along with State. This will
-- help with the implementation of store and recall
-- type synonymn for the exported values
--
type CalcState = State [Double]

-- For public exported values
type Calculation = CalcState ()

-- HP calculators, in addition to the stack,  also contained a memory location as a
-- part of their state.  Let's suppose we wanted to implement  the store and recall
-- functionality.

data InternalState = InternalState { stack  :: [Double]
                                   , memory :: Double
                                   }

-- Now update the CalcState using this record
-- This will break our implementation, but because of the abstraction
-- it will be limited to push pop and perform. Lets keep the older
-- versions cpop cpush and cperform as is and redefine new.
-- type CalcState = State InternalState
type CalculationState = State InternalState

-- Monadic values for the operations PUSH and POP from the stack

-- pop the result
cpop :: CalcState Double
cpop = do
      stk <- get
      case stk of
        []       -> return 0.0
        (x : xs) -> do
           put xs
           return x

pop :: CalculationState Double
pop = state $ \stk -> case stack stk of
                       [] -> (0.0, stk)
                       (x : xs) -> (x, stk { stack = xs })

-- push the values for calculation
cpush :: Double -> CalcState ()
cpush p = do
      stk <- get
      put (p : stk)

push :: Double -> CalculationState ()
push p = modify $ \stk -> stk { stack = p : stack stk }

-- The push operation, which we think of as a stack primitive, is actually
-- something we want to export, albeit under a different name
cEnter :: Double -> CalcState ()
cEnter = cpush

-- basic arithmetic operations
binOp :: (Double -> Double -> Double) -> CalcState ()
binOp op = do
      y <- cpop
      x <- cpop
      cpush $ op x y

-- operations using the above function
cAdd, cSub, cDiv, cMul :: CalcState ()
cAdd = binOp (+)
cSub = binOp (-)
cDiv = binOp (/)
cMul = binOp (*)

-- code to run the actual calculations
cperform :: Calculation -> Double
cperform ma = fst $ runState (ma >> cpop) []

perform :: Calculation -> Double
perform ma = evalState (ma >> pop) startState where
             startState = InternalState { stack = [], memory = 0.0 }

-- calculations for RPN
-- computing (1 + 2) * 3
test :: Double
test = perform $ do
     cEnter 1
     cEnter 2
     cAdd
     cEnter 3
     cMul

-- swapping the values
cSwap :: Calculation
cSwap = do
       y <- cpop
       x <- cpop
       cpush y
       cpush x

-- duplication of values
cDup :: Calculation
cDup = do
      x <- cpop
      cpush x
      cpush x

-- square root function
-- define a private helper unary function first
unyOp :: (Double -> Double) -> Calculation
unyOp op = do
      x <- cpop
      cpush (op x)

cSqrt :: Calculation
cSqrt = unyOp sqrt

-- Trigonometric functions
cSin, cCos, cTan :: Calculation
cSin = unyOp sin
cCos = unyOp cos
cTan = unyOp tan

-- square of a number
square :: Calculation
square = do
       cDup
       cMul

-- hypotenus of a right triangle
hypotenuse :: Calculation
hypotenuse = do
           square
           cSwap
           square
           cAdd
           cSqrt

-- λ> perform $ cEnter 3 >> cEnter 4 >> hypotenuse
-- 5.0
------------------------------------------------------------------------
