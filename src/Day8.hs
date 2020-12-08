{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Day8 where

import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import Util (SomeDay(..), Day, Task, runTask, safeIndex)

someDay8 :: SomeDay
someDay8 = SomeDay day8

day8 :: Day 8 ()
day8 = do
    runTask day8Task1
    runTask day8Task2

day8Task1 :: Task 1 Int
day8Task1 = pure $ runProgram 0 [] 0 input

day8Task2 :: Task 2 [Int]
day8Task2 =
    pure
        $ mapMaybe
              ( runProgramUntilTermination 0 [] 0
              . (\(i, input) -> mapAtIndex i toggleJmpNop input)
              )
        $ zip [0 ..]
        $ replicate (length input) input

rawInput :: String
rawInput = $(embedStringFile "input/day8.txt")

input :: Program
input = either (error "Couldn't parse input") id $ parseInput rawInput

type ProgramPointer = Int
type Events = [ProgramPointer]
type Accumulator = Int

type Program = [Instruction]

data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int
    deriving (Show)

runProgram :: ProgramPointer -> Events -> Accumulator -> Program -> Int
runProgram pp events acc p
    | loopDetected = acc
    | otherwise    = nextStep runProgram pp events acc p
    where loopDetected = pp `elem` events

mapAtIndex :: Int -> (a -> a) -> [a] -> [a]
mapAtIndex targetIdx f xs = fmap snd $ go $ zip [0 ..] xs
  where
    go [] = []
    go ((i, y) : ys)
        | i == targetIdx = (i, f y) : ys
        | otherwise      = (i, y) : go ys

runProgramUntilTermination :: ProgramPointer
                           -> Events
                           -> Accumulator
                           -> Program
                           -> Maybe Int
runProgramUntilTermination pp events acc p
    | atEnd        = Just acc
    | loopDetected = Nothing
    | otherwise    = nextStep runProgramUntilTermination pp events acc p
  where
    loopDetected = pp `elem` events
    atEnd        = pp == length p

nextStep :: (ProgramPointer -> Events -> Accumulator -> Program -> a)
         -> ProgramPointer
         -> Events
         -> Accumulator
         -> Program
         -> a
nextStep fn pp events acc p = case safeIndex pp p of
    Just (Acc n) -> fn (pp + 1) (pp : events) (acc + n) p
    Just (Jmp n) -> fn (pp + n) (pp : events) acc p
    Just (Nop _) -> fn (pp + 1) (pp : events) acc p
    Nothing      -> error "ProgramPointer outside of program"

toggleJmpNop :: Instruction -> Instruction
toggleJmpNop = \case
    Jmp arg -> Nop arg
    Nop arg -> Jmp arg
    x       -> x

parseInput :: String -> Either String Program
parseInput = traverse parseInstruction . filter (/= "") . lines

parseInstruction :: String -> Either String Instruction
parseInstruction x
    | cmd == "nop "
    = first ("Cannot parse argument for nop: " <>) $ Nop <$> parseArgument arg
    | cmd == "jmp "
    = first ("Cannot parse argument for jmp: " <>) $ Jmp <$> parseArgument arg
    | cmd == "acc "
    = first ("Cannot parse argument for acc: " <>) $ Acc <$> parseArgument arg
    | otherwise
    = Left "Unrecognized command"
    where (cmd, arg) = splitAt 4 x

parseArgument :: String -> Either String Int
parseArgument x = case readMaybe num of
    Nothing -> Left $ "Cannot read argument numbers: " <> num
    Just x' -> Right $ multiple * x'
  where
    (sign, num) = splitAt 1 x
    multiple    = if sign == "+" then 1 else -1
