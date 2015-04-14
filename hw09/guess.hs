import System.Random(randomRIO)
import System.IO
import Control.Monad
import System.Exit

{-
Реализуйте следующую программу.
Программа загадывает число от 1 до 100, пользователь должен отгадать его.
После каждой попытки программа говорит больше ее число или меньше.
Если пользователь не отгадал за 5 попыток, то проигрыш, иначе победа.
(1.5 балла)
-}

runTurn :: Integer -> Integer -> IO ()
runTurn x _ = do
    putStr "Try guess the number:"
    y <- readLn :: IO Integer
    case signum (x - y) of -1 -> putStrLn "less"
                           1 -> putStrLn "greater"
                           0 -> putStrLn "you right" >> exitSuccess
    return ()

main :: IO ()
main = do
    x <- randomRIO (1 :: Integer, 100 :: Integer)
    forM_ [1..5] $ runTurn x
    putStrLn "you lose"
    return ()
