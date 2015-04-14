import Control.Exception(catch, IOException)
import System.Environment
import Data.List
import System.IO
import Control.Monad

{-
grep принимает строку и от 0 и больше имен файлов, выводит строки, в которых встречается как подстрока переданная первым параметром строчка.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

treatException :: IOException -> IO String
treatException e = do
    let err = show e
    hPutStr stderr ("Warning: Couldn't open " ++ err ++ "\n")
    return ""


findPatten :: String -> String -> IO ()
findPatten pattern fileName = do
    content <- catch (readFile fileName) treatException
    let linesOfFile = lines content
    let matched = filter (isInfixOf pattern) linesOfFile
    forM_ matched putStrLn
    return ()
 

main :: IO ()
main = do
    (pattern:fileNames) <- getArgs
    forM_ fileNames (findPatten pattern)
    return ()
