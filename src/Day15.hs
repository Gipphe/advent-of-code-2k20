{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Data.FileEmbed (embedStringFile)
import Control.DeepSeq (force)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M

import Util (SomeDay(..), Day, Task, runTask, split)

someDay15 :: SomeDay
someDay15 = SomeDay day15

day15 :: Day 15 ()
day15 = do
    runTask day15Task1
    runTask day15Task2

parseInput :: String -> [Int]
parseInput = fmap read . split ','

rawInput :: String
rawInput = $(embedStringFile "input/day15.txt")

parsedInput :: [Int]
parsedInput = parseInput rawInput

day15Task1 :: Task 1 Int
day15Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [Int] -> Int
computeTask1 = compute 2020

day15Task2 :: Task 2 Int
day15Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [Int] -> Int
computeTask2 = compute 30000000

compute :: Int -> [Int] -> Int
compute _     []  = error "No numbers"
compute limit xs' = go limit startTurn x' $ M.fromList $ fmap (fmap pure) $ zip
    xs'
    [1 ..]
  where
    startTurn = length xs' + 1
    x'        = last xs'

go :: Int
   -- ^ Turn limit
   -> Int
   -- ^ Current turn number
   -> Int
   -- ^ Current number that we are evaluating
   -> IntMap [Int]
   -- ^ Map of previously spoken numbers and the turns they were spoken
   -> Int
go limit !currentTurn !x !m
    | currentTurn <= limit = go limit (currentTurn + 1) newNumber m'
    | otherwise            = x
  where
    newNumber = maybe 0 handleNewNumber $ M.lookup x m
    m'        = force $ M.insertWith (<>) newNumber [currentTurn] m
    handleNewNumber (lastTurn : turnBefore : _) = lastTurn - turnBefore
    handleNewNumber _                           = 0
