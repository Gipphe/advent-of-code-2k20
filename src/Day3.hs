{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Day3 where

import Data.FileEmbed (embedStringFile)

import Util (Day, SomeDay(..), Task, runTask)

someDay3 :: SomeDay
someDay3 = SomeDay day3

day3 :: Day 3 ()
day3 = do
    runTask day3Task1
    runTask day3Task2

parseInput :: String -> [String]
parseInput = lines

rawInput :: String
rawInput = $(embedStringFile "input/day3.txt")

parsedInput :: [String]
parsedInput = parseInput rawInput

day3Task1 :: Task 1 Int
day3Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [String] -> Int
computeTask1 = countTrees task1Step

day3Task2 :: Task 2 Int
day3Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [String] -> Int
computeTask2 input = product $ (`countTrees` input) <$> steps
  where
    steps :: [Step]
    steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

type Step = (Int, Int)

task1Step :: Step
task1Step = (3, 1)

slope :: Step -> [String] -> String
slope (stepX, stepY) forest = uncurry findTree <$> take rows slopePlan
  where
    slopePlan = iterate (\(y, x) -> (y + stepX, x + stepY)) (0, 0)
    rows      = length forest `div` stepY
    width     = length (head forest)
    findTree y x = (forest !! x) !! (y `mod` width)

countTrees :: Step -> [String] -> Int
countTrees step forest = length $ filter ('#' ==) $ slope step forest
