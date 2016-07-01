module TPrinter (main) where

{-|
   Author      : Sampath
   Maintainer  :
   File        : TPrinter.hs
   Description : A Simple directory Tree Printer in Haskell
                 The final tree will be printed as below

                 rootdir
                 |-- file1
                 |-- subdir1
                 |   |-- subfile11
                 |   |-- subfile12
                 |   `-- subdir13         -> node
                 |       `-- subfile131   -> node
                 `-- filen                -> node

                 Here the extreme vertical bar (|) at the left is the parent
                 The horizontal bar (--) is a link connecting all
                 The other vertical bar (|) is the arm. For the final file or
                 directory, it would be just a back quote (`)

   Compilation : ghc -O -o tprinter --make TPrinter.hs
   Usage       : ./tprinter [dir...]
-}

-- imports
----------------------------------------------------------------------
-- import           Control.Monad
-- import qualified Data.List          as L
-- import           System.Directory
import           System.Environment

----------------------------------------------------------------------
-- browse the file system
--
--browse
browse path parent link arm node = do
       putStrLn (parent ++ arm ++ link ++ node)
       browseChild (path ++ "/" ++ node) (parent ++ next)
       where
       next = case arm of
                "" -> undefined ""
                "`" -> "    "
                _ -> "|   "

browseChild path parent =

-- treeList function scans all the directories and then prints the
-- the tree structure. It does IO operations and is impure
--
treeList = undefined

----------------------------------------------------------------------
-- main method

main :: IO ()
main = do
     args <- getArgs
     mapM_ treeList (if null args
                        then ["."]
                        else args)
----------------------------------------------------------------------
