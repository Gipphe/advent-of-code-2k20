{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}

module Day10 where

import Data.FileEmbed (embedStringFile)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as Seq

import Util (SomeDay(..), Day, Task, runTask)

someDay10 :: SomeDay
someDay10 = SomeDay day10

day10 :: Day 10 ()
day10 = do
    runTask day10Task1
    runTask day10Task2

parseInput :: String -> Jolts
parseInput = Seq.sort . Seq.fromList . fmap read . lines

rawInput :: String
rawInput = $(embedStringFile "input/day10.txt")

parsedInput :: Jolts
parsedInput = parseInput rawInput

day10Task1 :: Task 1 Int
day10Task1 = pure $ computeTask1 parsedInput

computeTask1 :: Jolts -> Int
computeTask1 = multiplyJolts . addDeviceJolt . findDiffs 0
  where
    addDeviceJolt = (3 <|)
    multiplyJolts jolts =
        length (Seq.filter (== 1) jolts) * length (Seq.filter (== 3) jolts)

day10Task2 :: Task 2 Int
day10Task2 = pure $ computeTask2 parsedInput

computeTask2 :: Jolts -> Int
computeTask2 = go 0 $ Seq.singleton 1
  where
    go _ Empty     Empty      = 0
    go _ (a :<| _) Empty      = a
    go i as        (l :<| ls) = go l as' ls
      where
        diff = l - i - 1
        s    = sum $ Seq.take (3 - diff) as
        as'  = foldr (<|) as $ s <| Seq.replicate diff 0

findDiffs :: Jolt -> Jolts -> Seq Int
findDiffs currentJolt = \case
    j :<| js -> if j - currentJolt == 1
        then 1 <| findDiffs j js
        else 3 <| findDiffs j js
    Empty -> mempty

type Jolts = Seq Jolt
type Jolt = Int
