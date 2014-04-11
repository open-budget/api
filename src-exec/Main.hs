module Main where

import OpenBudget.Builder

main :: IO ()
main = newDatabase >>= print
