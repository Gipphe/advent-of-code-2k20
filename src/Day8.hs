{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Day8 where

import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as S

import Util (SomeDay(..), Day, Task, runTask)

someDay8 :: SomeDay
someDay8 = SomeDay day8

day8 :: Day 8 ()
day8 = do
    runTask day8Task1
    runTask day8Task2

parseInput :: String -> Program
parseInput = either (error "Couldn't parse input") id . parseProgram

rawInput :: String
rawInput = $(embedStringFile "input/day8.txt")

parsedInput :: Program
parsedInput = parseInput rawInput

day8Task1 :: Task 1 Int
day8Task1 = pure $ computeTask1 parsedInput

computeTask1 :: Program -> Int
computeTask1 = runProgram 0 mempty 0

day8Task2 :: Task 2 Int
day8Task2 = pure $ computeTask2 parsedInput

computeTask2 :: Program -> Int
computeTask2 input =
    head
        $ mapMaybe
              ( runProgramUntilTermination 0 mempty 0
              . (\(i, program) -> mapAtIndex i toggleJmpNop program)
              )
        $ zip [0 ..]
        $ replicate (length input) input

type ProgramPointer = Int
type Events = Set ProgramPointer
type Accumulator = Int

type Program = Vector Instruction

data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int
    deriving (Show)

runProgram :: ProgramPointer -> Events -> Accumulator -> Program -> Int
runProgram pp events acc p
    | loopDetected = acc
    | otherwise    = nextStep runProgram pp events acc p
    where loopDetected = pp `S.member` events

mapAtIndex :: Int -> (a -> a) -> Vector a -> Vector a
mapAtIndex targetIdx f = Vec.imap $ \i x -> if i == targetIdx then f x else x

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
    loopDetected = pp `S.member` events
    atEnd        = pp == length p

nextStep :: (ProgramPointer -> Events -> Accumulator -> Program -> a)
         -> ProgramPointer
         -> Events
         -> Accumulator
         -> Program
         -> a
nextStep fn pp events acc p = case p !? pp of
    Just (Acc n) -> fn (pp + 1) (S.insert pp events) (acc + n) p
    Just (Jmp n) -> fn (pp + n) (S.insert pp events) acc p
    Just (Nop _) -> fn (pp + 1) (S.insert pp events) acc p
    Nothing      -> error "ProgramPointer outside of program"

toggleJmpNop :: Instruction -> Instruction
toggleJmpNop = \case
    Jmp arg -> Nop arg
    Nop arg -> Jmp arg
    x       -> x

parseProgram :: String -> Either String Program
parseProgram =
    traverse parseInstruction . Vec.fromList . filter (/= "") . lines

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
