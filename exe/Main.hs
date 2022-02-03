module Main where

import Hangman ( randomWord'
               , freshPuzzle
               , runGame)
               
import Data.Char ( toLower )

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
