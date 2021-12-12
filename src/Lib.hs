module Lib
    ( interact'
    ) where

interact' :: (Show a, Read a) => ([a] -> a) -> String -> String
interact' solve = show . solve . numbers
    where numbers = map read . lines