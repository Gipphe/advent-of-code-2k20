{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Day14 where

import Control.Applicative ((<|>))
import Data.FileEmbed (embedStringFile)
import Data.Void (Void)
import Data.Char (digitToInt)
import Data.Bifunctor (first)
import Data.Bits ((.&.), (.|.), setBit, clearBit)
import Data.Functor (($>))
import Data.IntMap.Strict (insert)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl')
import Text.Megaparsec (Parsec, errorBundlePretty, parse, count, some, sepEndBy)
import Text.Megaparsec.Char (char, string, digitChar, eol)

import Util (SomeDay(..), Day, Task, runTask)

someDay14 :: SomeDay
someDay14 = SomeDay day14

day14 :: Day 14 ()
day14 = do
    runTask day14Task1
    runTask day14Task2

parseInput :: String -> [Instr]
parseInput = either error id . parseInstructions

rawInput :: String
rawInput = $(embedStringFile "input/day14.txt")

parsedInput :: [Instr]
parsedInput = parseInput rawInput

day14Task1 :: Task 1 Int
day14Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [Instr] -> Int
computeTask1 (NewBitMask firstBitmask : instrutions) =
    foldl' (+) 0 . snd $ foldl' go (firstBitmask, mempty) instrutions
  where
    go (!bitmask, !res) = \case
        NewBitMask mask -> (mask, res)
        WriteBits addr val ->
            (bitmask, insert addr (applyMask bitmask val) res)
    applyMask mask v = v .&. zeroMask .|. oneMask
      where
        oneMask  = toDec $ maybeOne <$> mask
        zeroMask = toDec $ maybeZero <$> mask
computeTask1 _ = error "Missing first bitmask"

day14Task2 :: Task 2 Int
day14Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [Instr] -> Int
computeTask2 (NewBitMask firstBitmask : instructions) =
    foldl' (+) 0 . snd $ foldl' go (firstBitmask, mempty) instructions
  where
    go (!bitmask, !res) = \case
        NewBitMask mask -> (mask, res)
        WriteBits addr val ->
            ( bitmask
            , foldl' (\acc key -> insert key val acc) res
                $ maskToAddrs bitmask addr
            )
computeTask2 _ = error "Missing first bitmask"

maskToAddrs :: BitMask -> Int -> Set Int
maskToAddrs mask addr = fanOutMask 35 mask $ addr .|. oneMask
  where
    oneMask  = toDec $ maybeOne <$> mask

fanOutMask :: Int -> BitMask -> Int -> Set Int
fanOutMask _ [] n = S.singleton n
fanOutMask bitIdx (X : rest) n =
    fanOutMask (bitIdx - 1) rest (n `setBit` bitIdx)
        `S.union` fanOutMask (bitIdx - 1) rest (n `clearBit` bitIdx)
fanOutMask bitIdx (_ : rest) n = fanOutMask (bitIdx - 1) rest n

maybeOne :: TriBit -> Char
maybeOne One = '1'
maybeOne _   = '0'

maybeZero :: TriBit -> Char
maybeZero Zero = '0'
maybeZero _    = '1'

isX :: TriBit -> Bool
isX X = True
isX _ = False

parseInstructions :: String -> Either String [Instr]
parseInstructions = first errorBundlePretty . parse instrsP ""

instrsP :: Parser [Instr]
instrsP = sepEndBy instrP eol

instrP :: Parser Instr
instrP = newBitmaskP <|> writeBitsP

newBitmaskP :: Parser Instr
newBitmaskP = do
    _    <- string "mask = "
    mask <- bitmaskP
    pure $ NewBitMask mask

writeBitsP :: Parser Instr
writeBitsP = do
    _    <- string "mem["
    addr <- read <$> some digitChar
    _    <- string "] = "
    val  <- read <$> some digitChar
    pure $ WriteBits addr val

bitmaskP :: Parser BitMask
bitmaskP = count 36 bitP

bitP :: Parser TriBit
bitP = char '1' $> One <|> char '0' $> Zero <|> char 'X' $> X

bitChar :: Parser Char
bitChar = char '1' <|> char '0'

data Instr
    = NewBitMask !BitMask
    | WriteBits !Int !Int

type BitMask = [TriBit]

data TriBit = One | Zero | X
    deriving (Eq)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

type Parser = Parsec Void String
