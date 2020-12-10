{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Day1 where

import Data.FileEmbed (embedStringFile)
import Data.List (find)


import Util (Day, Task, SomeDay(..), runTask)

parseInput :: String -> [Int]
parseInput = fmap read . lines

rawInput :: String
rawInput = $(embedStringFile "input/day1.txt")

parsedInput :: [Int]
parsedInput = parseInput rawInput

someDay1 :: SomeDay
someDay1 = SomeDay day1

day1 :: Day 1 ()
day1 = do
    runTask day1Task1
    runTask day1Task2

day1Task1 :: Task 1 Int
day1Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [Int] -> Int
computeTask1 input = case foldr findMatch Nothing input of
    Nothing  -> error "Did not find an answer"
    Just res -> res
  where
    findMatch x = \case
        Nothing -> (* x) <$> find (\y -> x + y == 2020) input
        res     -> res

day1Task2 :: Task 2 Int
day1Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [Int] -> Int
computeTask2 input = case theAnswer of
    Nothing  -> error "Did not find an answer"
    Just res -> res
  where
    allInputs = [ (x, y, z) | x <- input, y <- input, z <- input ]
    theAnswer =
        (\(x, y, z) -> x * y * z)
            <$> find (\(x, y, z) -> x + y + z == 2020) allInputs
