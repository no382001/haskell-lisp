module Main where --create module
import System.Environment --which uses Environment

main :: IO()
main = do
    (arg1:arg2:_) <- getArgs
    let result = read(arg1) + read(arg2)
    putStrLn(show(result))