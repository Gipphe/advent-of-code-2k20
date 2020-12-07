{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Day1 where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)

import Util (Day, Task, SomeDay(..), runTask)

ioInput :: IO [Int]
ioInput = fmap read . lines <$> readFile "input/day1.txt"

someDay1 :: SomeDay
someDay1 = SomeDay day1

day1 :: Day 1 ()
day1 = do
    runTask day1Task1
    runTask day1Task2

day1Task1 :: Task 1 Int
day1Task1 = do
    input <- liftIO ioInput
    let
        findMatch x = \case
            Nothing -> (* x) <$> find (\y -> x + y == 2020) input
            res     -> res
    case foldr findMatch Nothing input of
        Nothing  -> error "Did something wrong"
        Just res -> pure res

day1Task2 :: Task 2 Int
day1Task2 = do
    input <- liftIO ioInput
    let allInputs = [ (x, y, z) | x <- input, y <- input, z <- input ]
        theAnswer =
            (\(x, y, z) -> x * y * z)
                <$> find (\(x, y, z) -> x + y + z == 2020) allInputs
    case theAnswer of
        Nothing  -> error "Did something wrong"
        Just res -> pure res
