module Main where

import Person
import Lib


main :: IO ()
main = do
 
  putStrLn .show $ Person{name = "扶摇" ,age = 25}
