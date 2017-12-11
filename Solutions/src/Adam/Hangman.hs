module Hangman where

import           Control.Monad
import           Data.Char
import           Data.List
import           System.Exit

data Game = Game
    { word    :: String
    , guessed :: String
    } deriving (Show, Eq)

guess :: Char -> Game -> Game
guess c (Game w g) = Game w g'
    where g' = if c `elem` g
            then g
            else c:g

display :: Game -> String
display (Game w g) = intersperse ' ' $ f <$> w
    where f c = if c `elem` g
            then c
            else '_'

failed :: Game -> Int
failed (Game w g) = length . filter f $ g
    where f = not . (`elem` w)

remaining :: Game -> Int
remaining (Game w g) = foldr f 0 w
    where f c a = if c `elem` g
            then a
            else a + 1

main :: IO ()
main = gameLoop $ Game "jennifer" ""

gameLoop :: Game -> IO ()
gameLoop game@(Game w g) = forever $ do
    putStrLn $ display game

    if remaining game == 0
        then do
            putStrLn "Win!"
            exitSuccess
        else pure ()

    if failed game == 10
        then do
            putStrLn "Lose!"
            exitSuccess
        else pure()

    putStr "Provide a letter: "
    l <- getLine

    let validate = do
            c <- if length l /= 1
                then Left "Just one character, please"
                else Right $ toLower . head $ l

            if c `elem` g
                then Left "Already guessed"
                else Right c

    either err next validate >>= gameLoop
        where
            err e = do
                putStrLn e
                pure game

            next c = pure $ guess c game
