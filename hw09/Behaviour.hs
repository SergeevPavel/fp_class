{-
Реализуйте runBehaviour
(1.5 балла)
-}
import Control.Monad
import System.IO
import Data.List
import System.IO.Unsafe

data Request  = Get           | Put String deriving Show
data Response = Result String | OK deriving Show

type Behaviour = [Response] -> [Request]

prog :: Behaviour
prog ~(OK : x : xs) = Put "more? " : Get : case x of
    Result "no" -> []
    Result "yes" -> prog xs
    _ -> Put "yes or no!" : prog (tail xs)

executeRequest :: Request -> IO Response
executeRequest Get = do
    input <- getLine 
    return $ Result input

executeRequest (Put output) = do
    putStrLn output
    return OK

runBehaviour :: Behaviour -> IO ()
runBehaviour behaviour = do
    let responses = map (unsafePerformIO . executeRequest) . behaviour $ responses
    foldr seq (return ()) responses

main = runBehaviour prog
