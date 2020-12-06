{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Day3
    ( someDay3
    )
where

import Control.Monad.IO.Class (liftIO)

import Util (SomeDay(..), Day, runTask, Task)

someDay3 :: SomeDay
someDay3 = SomeDay day3

day3 :: Day 3 ()
day3 = do
    runTask day3Task1
    runTask day3Task2

ioInput :: IO [String]
ioInput = lines <$> readFile "input/day3.txt"

day3Task1 :: Task 1 Int
day3Task1 = countTrees task1Step <$> liftIO ioInput

day3Task2 :: Task 2 Int
day3Task2 = do
    input <- liftIO ioInput
    pure $ product $ (`countTrees` input) <$> steps
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
