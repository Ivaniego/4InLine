module Main where

import Lib


main :: IO ()
main = putStrLn "Hello World"

transformHead :: (a -> a) -> [a] -> a -> [a]
transformHead f (x : xs) n = n : xs