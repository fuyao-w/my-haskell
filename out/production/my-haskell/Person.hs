module Person(
    Person(..)
    ,getSelf
) where

import qualified Data.Map as M


data Person = Person {
  name :: String,
  age ::  Int 
} deriving (Show)

getPersonMap :: M.Map [Char] Int
getPersonMap = M.fromList [("wfy",25),("bst",26)]


getSelf :: String -> Maybe Int 
getSelf myname = M.lookup myname getPersonMap