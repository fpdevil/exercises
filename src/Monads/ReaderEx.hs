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
import           System.Environment
import           System.IO

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
      -- again ask for the environment if access is needed
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
----------------------------------------------------------------------
-- another example
-- accessing application configuration – and working our way to effectful
-- bliss, starting with the  first principles

data AppConfiguration = AppConfiguration { logFile      :: FilePath
                                         , version      :: String
                                         , maxMsgLength :: Int
                                         } deriving (Show, Read)

-- At first this configuration will be shared throughout the application
-- by passing the configuration to every function that needs it. The
-- following are 2 contrived examples of the same.

-- first, Initializes an application log file handle. It needs the log
-- file path and the version from the config.
-- opens a handle as specified in the config and writer preamble
initLogFile :: String -> AppConfiguration -> IO Handle
initLogFile preamble config = do
    handle <- openFile (logFile config) WriteMode
    hPutStrLn handle (preamble ++ ", version: " ++ version config)
    return handle

-- the application deals with messages under a certain max length
-- the validateMsg function will enforce such a condition
validateMsg :: String -> AppConfiguration -> Either String ()
validateMsg msg config = if length msg > maxMsgLength config
                         then Left ("Message too long " ++ msg)
                         else Right ()

-- In both the above functions we are passong the AppConfiguration
-- as an argument. This is the manual way of doing things.

-- specify a type synonymn for a function which takes the AppConfiguration
-- value and returns an a
type ConfigReader a = AppConfiguration -> a

initLogFileTS :: String -> ConfigReader (IO Handle)
initLogFileTS = initLogFile

validateMsgTS :: String -> ConfigReader (Either String ())
validateMsgTS = validateMsg

-- using the above kind of formalization, we get the explicit argument
-- out of the signture.
