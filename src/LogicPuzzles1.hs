module LogicPuzzles1 where

import           Control.Monad


-- Structure And Interpretation of Computer Programs (SICP)

-- Logic Puzzles Ex 4.3.2

-- Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment
-- house that contains only five floors. Baker does not live on the top floor. Cooper
-- does not live on the bottom floor. Fletcher does not live on either the top or the
-- bottom floor. Miller lives on a higher floor than does Cooper. Smith does not live on
-- a floor adjacent to Fletcher’s. Fletcher does not live on a floor adjacent to Cooper’s.
-- Where does everyone live?


type Name  = String
type Floor = Int

isAdjacent :: Floor -> Floor -> Bool
isAdjacent floorx floory = abs (floorx - floory) == 1

isDistinct :: [Floor] -> Bool
isDistinct []  = True
isDistinct (x : xs) = x `notElem` xs && isDistinct xs

checkFloors :: [[(Name, Floor)]]
checkFloors = do
            baker    <- [1 .. 5]
            cooper   <- [1 .. 5]
            fletcher <- [1 .. 5]
            miller   <- [1 .. 5]
            smith    <- [1 .. 5]
            guard (isDistinct [baker, cooper, fletcher, miller, smith])
            guard (baker /= 5)
            guard (cooper /= 1)
            guard (fletcher /= 1)
            guard (fletcher /= 5)
            guard (miller > cooper)
            guard (not (isAdjacent smith fletcher))
            guard (not (isAdjacent fletcher cooper))
            return [("Baker", baker),
                    ("Cooper", cooper),
                    ("Fletcher", fletcher),
                    ("Miller", miller),
                    ("Smith", smith)
                   ]

-- Knight's Tour
-- Determine if a Knight can reach a certain poition in N moves
type KnightPosition = (Int, Int)

-- All possible positions to which a Knight can move legally
knightMoves :: KnightPosition -> [KnightPosition]
knightMoves (col, row) = do
            (c, r) <- [ (col + 1, row + 2), (col + 1, row - 2)
                     , (col - 1, row + 2), (col - 1, row - 2)
                     , (col + 2, row + 1), (col + 2, row - 1)
                     , (col - 2, row + 1), (col - 2, row - 1)
                     ]
            guard (c `elem` [1 .. 8] && r `elem` [1 .. 8])
            return (c, r)

-- calculate all the possible moves a knight can move to in
-- in N moves. For each next move it calculates all the possible
-- move from each of the previous results.
-- For 3 moves...
in3 :: KnightPosition -> [KnightPosition]
in3 start = do
    first  <- knightMoves start
    second <- knightMoves first
    -- for the third move
    knightMoves second

-- using bind >>=
in3' :: KnightPosition -> [KnightPosition]
in3' start = return start >>= knightMoves >>= knightMoves >>= knightMoves

-- Move the knight for n times
inMany :: Int -> KnightPosition -> [KnightPosition]
inMany x start = return start >>= foldr (<=<) return (replicate x knightMoves)

-- check if reached in 3 moves
-- enough to check if the result from above contains the final
-- propsed place. If it contains then knight can move in 3 steps
canMoveIn3 :: KnightPosition -> KnightPosition -> Bool
canMoveIn3 start end = end `elem` in3 start

-- can reach in N moves
canReachInN :: Int -> KnightPosition -> KnightPosition -> Bool
canReachInN x start end = end `elem` inMany x start
