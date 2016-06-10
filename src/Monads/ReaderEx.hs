module ReaderEx where

{--|
   Author      : Sampath
   Maintainer  :
   File        : ReaderEx.hs
   Description : Examples in Reader Monad.
                 In  the Immutable  Context,  if a  function wants  to
                 access  some   global  variables,   some  application
                 managers  or  services,  it cannot  pass  context  to
                 function
--}

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Char            as C

----------------------------------------------------------------------
--
-- example using the guard
filterX  :: (Enum b, Num b, Show b) => Int -> [b]
filterX n = [1 .. 100] >>= (\x -> guard (C.intToDigit n `elem` show x) >> return x)

----------------------------------------------------------------------
--
-- An example without Reader Monad
data Environment = Environment { name    :: String
                               , ids     :: [Int]
                               , manager :: String -> String
                               }

containsId :: Int -> Environment -> Bool
containsId i env = i `elem` ids env

safeCallManager :: Int -> Environment -> Maybe String
safeCallManager i env = if containsId i env
                           then Just $ manager env $ name env
                           else Nothing

-- λ> safeCallManager 1 $ Environment "Neural Networks" [1, 3] ("Welcome to " ++)
-- Just "Welcome to Neural Networks"
-- λ> safeCallManager 2 $ Environment "Neural Networks" [1, 3] ("Welcome to " ++)
-- Nothing
-- λ> safeCallManager 3 $ Environment "Neural Networks" [1, 3] ("Welcome to " ++)
-- Just "Welcome to Neural Networks"
----------------------------------------------------------------------
-- using reader monad for the above
containsIdR :: Int -> Reader Environment Bool
containsIdR i = do
            env <- ask
            return (i `elem` ids env)

-- containsIdR i = asks (\env -> i `elem` ids env)

safeCallManagerR :: Int -> Reader Environment (Maybe String)
safeCallManagerR i = containsIdR i >>= \hasId ->
                     if hasId
                        then asks (\env -> Just $ manager env $ name env)
                        else return Nothing

-- λ> runReader (safeCallManagerR 1) $ Environment "Jungle Adventure" [1 .. 3] ("Its a crazy crazy " ++)
-- Just "Its a crazy crazy Jungle Adventure"
-- λ> runReader (safeCallManagerR 4) $ Environment "Jungle Adventure" [1 .. 3] ("Its a crazy crazy " ++)
-- Nothing

----------------------------------------------------------------------
-- example for environment retrieval
tom :: Reader String String
tom = do
    -- if you need access to the environment you ask for it
    env <- ask
    return (env ++ " This is Tom the Cat.")

jerry :: Reader String String
jerry = do
      env <- ask
      return (env ++ " This is Jerry the Mouse.")

tomAndJerry :: Reader String String
tomAndJerry = do
            t <- tom
            j <- jerry
            return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this?"

-- λ> putStrLn runJerryRun
-- Who is this? This is Tom the Cat.
-- Who is this? This is Jerry the Mouse.
