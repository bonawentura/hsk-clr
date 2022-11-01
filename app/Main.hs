module Main where

import qualified MyLib (someFunc)
import Hangman

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
