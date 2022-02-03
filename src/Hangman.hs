module Hangman where

import Control.Monad (forever)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
      (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed


allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 5

incorrectGuess :: Int
incorrectGuess = 10

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength w = 
            let   l =  length (w :: String)
            in    l >= minWordLength
               && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle puzzleWord = Puzzle puzzleWord (fmap (const Nothing) puzzleWord) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle puzzleWord _ _) c = c `elem` puzzleWord

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessedWord) c = c `elem` guessedWord

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c:s)
  where zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
        newFilledInSoFar                  = zipWith (zipper c) word filledInSoFar
        

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of

    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick \
               \ something else!"
      return puzzle

    (True, _) -> do
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return (fillInCharacter puzzle guess)

    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)
       
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess ag guessed) =
  if (length guessed) > (incorrectGuess + (length $ filter isJust ag)) then
    do putStrLn "You Lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return () 

gameWin :: Puzzle -> IO ()
gameWin (Puzzle puzzleWord filledInSoFar _) = 
  if all isJust filledInSoFar then
    do putStrLn $ "\n\nYou win! \nYou sucessfully guessed the word was " ++ puzzleWord
       exitSuccess
   else return ()  

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn "\n"
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"


-- main :: IO ()
-- main = do
  -- word <- randomWord'
  -- let puzzle = freshPuzzle (fmap toLower word)
  -- runGame puzzle
