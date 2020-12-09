{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Day9 where

import Data.FileEmbed (embedStringFile)
import qualified Data.Set as S
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec
import Text.Read (readMaybe)

import Util (SomeDay(..), Day, Task, runTask)

someDay9 :: SomeDay
someDay9 = SomeDay day9

day9 :: Day 9 ()
day9 = do
    runTask day9Task1
    runTask day9Task2

rawInput :: String
rawInput = $(embedStringFile "input/day9.txt")

mInput :: (MonadFail m) => m (Vector Int)
mInput =
    maybe (fail "Could not read all numbers") pure
        . traverse readMaybe
        . Vec.fromList
        . lines
        $ rawInput

day9Task1 :: Task 1 Int
day9Task1 = either fail pure . check lim (lim + 1) =<< mInput where lim = 25

day9Task2 :: Task 2 Int
day9Task2 = do
    input  <- mInput
    target <- either fail pure $ check lim (lim + 1) input
    range  <-
        maybe (fail "Found no such contiguous run of numbers") pure
            $ checkContiguous target input
    pure $ maximum range + minimum range
    where lim = 25

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
checkContiguous target xs = go target 0 1 xs
  where
    go tgt start end xs
        | summed == tgt = Just currentSlice
        | summed > tgt  = go tgt (start + 1) end xs
        | summed < tgt  = go tgt start (end + 1) xs
        | otherwise     = Nothing
      where
        currentSlice = Vec.slice start (end - start) xs
        summed       = sum $ Vec.toList currentSlice
