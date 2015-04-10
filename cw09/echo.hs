import Control.Monad
import System.Environment
import Data.List

main = getArgs >>= \s -> putStrLn $ intercalate " " s
