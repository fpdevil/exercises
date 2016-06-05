module Example4 where

{--|
   Author      : Sampath
   Maintainer  :
   File        : Example4.hs
   Description : Various Example problems
--}


-- implementation of Pascals triangle
--
binomialCoefficients :: (Num a) => [a] -> [a]
binomialCoefficients xs = zipWith (+) (0 : xs) (xs ++ [0])

pascalsRows :: (Num a) => [[a]]
pascalsRows = iterate binomialCoefficients [1]

pascalsTriangle :: Int -> IO ()
pascalsTriangle n = mapM_ print $ take n pascalsRows
