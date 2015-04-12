import Control.Exception(catch, IOException)
import System.Environment
import System.IO
import Control.Monad
{-
cat принимает имена файлов и выводит их содержимое на экран.
Если в cat не передаются параметры, то она копирует stdin в stdout.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}


printFile :: String -> IO ()
printFile fileName = do
    content <- catch (readFile fileName)
        (\e -> do let err = show (e :: IOException)
                  hPutStr stderr ("Warning: Couldn't open " ++ fileName ++ ": " ++ err ++ "\n")
                  return "")
    putStr content


main :: IO ()
main = do
    fileNames <- getArgs
    if (length fileNames == 0) then forever (getLine >>= putStrLn)
                               else forM_ fileNames printFile
    
