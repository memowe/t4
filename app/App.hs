module Main where

import MyLib
import System.IO

main :: IO ()
main = do
  putStr "What's your name? "
  hFlush stdout
  name <- getLine
  putStrLn $ welcome name
