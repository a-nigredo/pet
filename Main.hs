module Main where

main :: IO ()
main = putStrLn (show (double 2))

double :: Int -> Int
double x = x + x
