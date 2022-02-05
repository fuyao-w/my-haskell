module Main where

import Person
import Lib
import System.Environment


main :: IO ()
main = do
  putStrLn .show $ Person{name = "扶摇" ,age = 25}


