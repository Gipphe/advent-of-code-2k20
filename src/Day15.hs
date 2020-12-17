{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Control.DeepSeq (force)
import Control.Monad.ST (ST, runST)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (foldlM, toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V

import Util (Day, SomeDay(..), Task, runTask, split)


----------------
-- * Boilerplate
----------------

someDay15 :: SomeDay
someDay15 = SomeDay day15

day15 :: Day 15 ()
day15 = do
    runTask day15Task1
    runTask day15Task2

day15Task1 :: Task 1 Int
day15Task1 = pure $ computeTask1 parsedInput

day15Task2 :: Task 2 Int
day15Task2 = pure $ computeTask2 parsedInput


---------
-- * Work
---------

computeTask1 :: Seq Int -> Int
computeTask1 = computeV 2020

computeTask2 :: Seq Int -> Int
computeTask2 = computeV 30000000

-- | Unboxed, mutable vector solution
computeV :: Int -> Seq Int -> Int
computeV limit (toList -> initial) = runST $ do
    vector <- V.new limit
    mapM_ (uncurry $ V.write vector) $ zip (init initial) [1 ..]
    foldlM (stepV vector) (last initial) [length initial .. limit - 1]

stepV :: MVector s Int -> Int -> Int -> ST s Int
stepV vector num counter = do
    lastCount <- V.read vector num
    V.write vector num counter
    if lastCount == 0 then pure 0 else pure $ counter - lastCount

-- | Previous solution
compute :: Int -> [Int] -> Int
compute _ [] = error "No numbers"
compute limit xs' =
    step limit startTurn x' $ M.fromList $ fmap (fmap pure) $ zip
        xs'
        [1 .. length xs']
  where
    startTurn = length xs' + 1
    x'        = last xs'

step :: Int -> Int -> Int -> IntMap [Int] -> Int
step limit !currentTurn !x !m
    | currentTurn <= limit = step limit (currentTurn + 1) newNumber m'
    | otherwise            = x
  where
    newNumber = maybe 0 handleNewNumber $ M.lookup x m
    m'        = force $ M.insertWith (<>) newNumber [currentTurn] m
    handleNewNumber (lastTurn : turnBefore : _) = lastTurn - turnBefore
    handleNewNumber _                           = 0


----------
-- * Input
----------

parseInput :: String -> Seq Int
parseInput = Seq.fromList . fmap read . split ','

rawInput :: String
rawInput = $(embedStringFile "input/day15.txt")

parsedInput :: Seq Int
parsedInput = parseInput rawInput
