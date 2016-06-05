module LogicPuzzles2 where

-----------------------------------------------------------------------
--                           Logic Puzzles 2
-----------------------------------------------------------------------
-- Solving the "Mr.S and Mr.P" puzzle by John McCarthy:

-- 	Formalization of two Puzzles Involving Knowledge
-- 	McCarthy, John (1987).
-- 	http://www-formal.stanford.edu/jmc/puzzles.html

-- We pick two numbers a and b,  so that a ≥ b and both numbers are within
-- the range [2,99]. We  give Mr.P the product a*b and  give Mr.S the sum
-- a + b.

-- The following dialog takes place:

-- 	Mr.P: I don't know the numbers
-- 	Mr.S: I knew you didn't know. I don't know either.
-- 	Mr.P: Now I know the numbers
-- 	Mr.S: Now I know them too

-- Can we find the numbers a and b?

-- The number conditions are 2 ≤ a ≤ b ≤ 99

-----------------------------------------------------------------------
-- first define valid numbers between 2 and 99
--
validNumbers :: [Int]
validNumbers = [2 .. 99]

-- Number factor table
-- Given  any number  p, find  all  the factors  a  and b  of the  number
-- satisfying the condition a >= b and a * b == p and return pairs (a, b)
--
factorPairsTable :: [[(Int, Int)]]
factorPairsTable = map funp [0 ..]
              where
              funp n = [(a, b) | a <- validNumbers, b<- validNumbers, a >= b, a * b == n]

factorPairs :: Int -> [(Int, Int)]
factorPairs 0 = []
factorPairs p = factorPairsTable !! p

-- Number Summands table
-- Given any number s, find all the summands a and b of the number
-- satisfying the condition a >= b and a + b == p and return pairs (a, b
--
summandPairsTable :: [[(Int, Int)]]
summandPairsTable = map funs [0 .. ]
                    where
                    funs n = [(a, b) | a <- validNumbers, b <- validNumbers, a >= b, a + b == n]

summandPairs :: Int -> [(Int, Int)]
summandPairs 0 = []
summandPairs s = summandPairsTable !! s

--
-- Check if a list contains a single element
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

-- condition 1
-- Mr.P: I don't know the numbers
-- P would have known the numbers if the product had unique valid factors
--
condition1 :: (Int, Int) -> Bool
condition1 (a, b) = not (isSingleton $ factorPairs (a * b))

-- conditions 2 and 3
-- Mr.S: I knew you didn't know. I don't know either.
-- condition2
-- S does not know the numbers
-- S would have known the numbers if the sum had unique valid summands
--
condition2 :: (Int, Int) -> Bool
condition2 (a, b) = not (isSingleton $ summandPairs (a + b))

--
-- condition3
-- Mr.S: I knew you didn't know.
-- S knows that P does not know the numbers
-- For all possible summands a and b which make (a + b)
-- Mr. P cannot be certain of the numbers
--
condition3 :: (Int, Int) -> Bool
condition3 (a, b) = all condition1 $ summandPairs (a + b)

--
-- condition 4
-- Mr.P: Now I know the numbers
-- Based on truthfulness of the condition 3, P now knows the numbers.
-- For all the product factors of (a * b) there exists only one value
-- which satisfies the condition3
--
condition4 :: (Int, Int) -> Bool
condition4 (a, b) = isSingleton $ filter condition3 (factorPairs (a * b))

--
-- condition 5
-- Mr.S: Now I know them too
-- S knows the fact that P knows the numbers
-- For all the combinations of the set (a + b), there is only
-- one comnination which satisfies the condition 4
--
condition5 :: (Int, Int) -> Bool
condition5 (a, b) = isSingleton $ filter condition4 (summandPairs (a + b))

--
-- Final result
-- Find the list of all numbers which satisfy the conditions 1 through 5
--
result :: [(Int, Int)]
result = [(a, b) | a <- validNumbers,
                   b <- validNumbers,
                   a >= b,
                   all ($ (a, b)) [condition1, condition2, condition3, condition4, condition5]
         ]

--
-- display results
--
main :: IO ()
main = print result

-- λ> main
-- [(13,4)]
-- (11.52 secs, 2,694,648,064 bytes)
