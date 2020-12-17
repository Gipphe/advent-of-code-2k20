{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Day6 where

import Data.FileEmbed (embedStringFile)
import Data.List (group, nub, sort)

import Util (Day, SomeDay(..), Task, runTask, splitOnDoubleNewline)

parseInput :: String -> [String]
parseInput = fmap (unwords . lines) . splitOnDoubleNewline

rawInput :: String
rawInput = $(embedStringFile "input/day6.txt")

parsedInput :: [String]
parsedInput = parseInput rawInput

someDay6 :: SomeDay
someDay6 = SomeDay day6

day6 :: Day 6 ()
day6 = do
    runTask day6Task1
    runTask day6Task2

day6Task1 :: Task 1 Int
day6Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [String] -> Int
computeTask1 = sum . fmap (length . nub . mconcat . words)

day6Task2 :: Task 2 Int
day6Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [String] -> Int
computeTask2 = sum . fmap countAnswers
  where
    countAnswers :: String -> Int
    countAnswers answers =
        length
            . filter id
            . fmap ((== personsInGroup) . length)
            . group
            . sort
            . mconcat
            . words
            $ answers
        where personsInGroup = length $ words answers
