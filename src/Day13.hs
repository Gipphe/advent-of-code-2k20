{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Day13 where

import Data.FileEmbed (embedStringFile)
import Data.Foldable (foldl')
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)

import Util (Day, SomeDay(..), Task, runTask, split)

someDay13 :: SomeDay
someDay13 = SomeDay day13

day13 :: Day 13 ()
day13 = do
    runTask day13Task1
    runTask day13Task2

parseInput :: String -> (Int, [Maybe Int])
parseInput (lines -> [target, times]) =
    (read target, readTime <$> split ',' times)
parseInput _ = error "Not two lines"

rawInput :: String
rawInput = $(embedStringFile "input/day13.txt")

parsedInput :: (Int, [Maybe Int])
parsedInput = parseInput rawInput

day13Task1 :: Task 1 Int
day13Task1 = pure $ computeTask1 parsedInput

day13Task2 :: Task 2 Integer
day13Task2 = pure $ computeTask2 parsedInput

computeTask1 :: (Int, [Maybe Int]) -> Int
computeTask1 (target, times) =
    uncurry (*)
        .   fmap (subtract target)
        .   head
        .   sortOn snd
        $   fmap head
        .   omitTooEarlyTimes
        .   iterateTime
        <$> catMaybes times
  where
    omitTooEarlyTimes :: (Int, [Int]) -> (Int, [Int])
    omitTooEarlyTimes = fmap (filter ((>= target)))

    iterateTime :: Int -> (Int, [Int])
    iterateTime x = (x, iterate (+ x) x)

computeTask2 :: (a, [Maybe Int]) -> Integer
computeTask2 = part2CRT . fmap (fmap toInteger) . snd

part2CRT :: [Maybe Integer] -> Integer
part2CRT =
    chineseRemSieve
        . fmap (\(a, n) -> (a `mod` n, n))
        . mapMaybe sequenceA
        . zip [0, -1 ..]

-- fst integer = lhs of mod
-- snd integer = rhs of mod
chineseRemSieve :: [(Integer, Integer)] -> Integer
chineseRemSieve = fst . foldl' f k . sortOn snd
  where
    k = (0, [1])
    f :: (Integer, [Integer]) -> (Integer, Integer) -> (Integer, [Integer])
    f (x, ns) (a, n') =
        (\x' -> (x', n' : ns))
            . fromJust
            . find (\x' -> x' `rem` n' == a)
            $ [x, x + product ns ..]

readTime :: String -> Maybe Int
readTime = \case
    "x" -> Nothing
    t   -> Just (read t)
