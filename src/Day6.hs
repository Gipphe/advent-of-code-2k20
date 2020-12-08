{-# LANGUAGE DataKinds #-}

module Day6 where

import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sort, group)

import Util (Day, SomeDay(..), Task, runTask, splitOnDoubleNewline, sumTrue)

ioInput :: IO [String]
ioInput =
    fmap (unwords . lines) . splitOnDoubleNewline <$> readFile "input/day6.txt"

someDay6 :: SomeDay
someDay6 = SomeDay day6

day6 :: Day 6 ()
day6 = do
    runTask day6Task1
    runTask day6Task2

day6Task1 :: Task 1 Int
day6Task1 = do
    sum . fmap (length . nub . mconcat . words) <$> liftIO ioInput

day6Task2 :: Task 2 Int
day6Task2 = do
    sum . fmap countAnswers <$> liftIO ioInput
  where
    countAnswers :: String -> Int
    countAnswers answers =
        sumTrue
            . fmap ((== personsInGroup) . length)
            . group
            . sort
            . mconcat
            . words
            $ answers
        where personsInGroup = length $ words answers
