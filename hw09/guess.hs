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
    -- y <- readLn :: IO Integer -- зачем аннотация типа? Он сам может его вывести.
    y <- readLn
    case compare x y of LT -> putStrLn "less"
                        GT -> putStrLn "greater"
                        EQ -> putStrLn "you right" >> exitSuccess

main :: IO ()
main = do
    x <- randomRIO (1, 100)
    forM_ [1..5] $ runTurn x
    putStrLn "you lose"
    -- return () -- зачем это здесь? return ничего не делает. Его нужно взывать только если нужно вернуть что-то отличное от того, что возвращает последняя строчка в do-списке.
