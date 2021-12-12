module Main where

import Lib
import Day1 (part1, part2)

main :: IO ()
main = interact (interact' part2)
