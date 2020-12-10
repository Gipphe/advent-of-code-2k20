{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Day9 where

import Data.FileEmbed (embedStringFile)
import qualified Data.Set as S
import Data.Vector (Vector, (!?))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import Text.Read (readMaybe)

import Util (SomeDay(..), Day, Task, runTask)

someDay9 :: SomeDay
someDay9 = SomeDay day9

day9 :: Day 9 ()
day9 = do
    runTask day9Task1
    runTask day9Task2

parseInput :: String -> Vector Int
parseInput =
    fromMaybe (error "Could not read all numbers")
        . traverse readMaybe
        . Vec.fromList
        . lines

rawInput :: String
rawInput = $(embedStringFile "input/day9.txt")

parsedInput :: Vector Int
parsedInput = parseInput rawInput

day9Task1 :: Task 1 Int
day9Task1 = pure $ computeTask1 parsedInput

computeTask1 :: Vector Int -> Int
computeTask1 = either error id . check lim (lim + 1) where lim = 25

day9Task2 :: Task 2 Int
day9Task2 = do
    pure $ computeTask2 parsedInput

computeTask2 :: Vector Int -> Int
computeTask2 input = maximum range + minimum range
  where
    lim    = 25
    target = either error id $ check lim (lim + 1) input
    range =
        fromMaybe (error "Found no such contiguous run of numbers")
            $ checkContiguous target input

type Pointer = Int
type PreviousNumbersLength = Int

check :: PreviousNumbersLength -> Pointer -> Vector Int -> Either String Int
check lim pp xs = do
    thisNumber <- maybe (Left "pp out of bounds") pure $ xs !? pp
    if thisNumber `S.member` possibleNextValues
        then check lim (pp + 1) xs
        else Right thisNumber
  where
    previousValues = Vec.slice (pp - lim) lim xs
    possibleNextValues =
        foldr S.insert S.empty
            .   Vec.map (\(x, _, _) -> x)
            .   Vec.filter (\(_, x, y) -> x /= y)
            $   (\x y -> (x + y, x, y))
            <$> previousValues
            <*> previousValues

checkContiguous :: Int -> Vector Int -> Maybe (Vector Int)
checkContiguous target = go target 0 1
  where
    go tgt start end xs
        | summed == tgt = Just currentSlice
        | summed > tgt  = go tgt (start + 1) end xs
        | summed < tgt  = go tgt start (end + 1) xs
        | otherwise     = Nothing
      where
        currentSlice = Vec.slice start (end - start) xs
        summed       = sum $ Vec.toList currentSlice
