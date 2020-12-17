{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Vector ((!?), Vector)
import qualified Data.Vector as Vec
import Data.Void (Void)
import Text.Megaparsec (Parsec, label, many, manyTill, parse, some)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, space)
import Text.Read (readMaybe)

import Util (Day, SomeDay(..), Task, runTask)

someDay2 :: SomeDay
someDay2 = SomeDay day2

day2 :: Day 2 ()
day2 = do
    runTask day2Task1
    runTask day2Task2

data Pw = Pw
    { pwFrom :: Int
    , pwTo   :: Int
    , pwChar :: Char
    , pwPw   :: Vector Char
    }

parseInput :: String -> [Pw]
parseInput = either error id . parsePws

rawInput :: String
rawInput = $(embedStringFile "input/day2.txt")

parsedInput :: [Pw]
parsedInput = parseInput rawInput

day2Task1 :: Task 1 Int
day2Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [Pw] -> Int
computeTask1 = length . filter id . fmap validatePw

day2Task2 :: Task 2 Int
day2Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [Pw] -> Int
computeTask2 = length . filter id . fmap validateWithRealPolicy

validatePw :: Pw -> Bool
validatePw Pw { pwFrom, pwTo, pwChar, pwPw } = isWithinLimits
    $ foldr countChar 0 pwPw
  where
    countChar c = if c == pwChar then (+ 1) else id
    isWithinLimits x = x >= pwFrom && x <= pwTo

validateWithRealPolicy :: Pw -> Bool
validateWithRealPolicy Pw { pwFrom, pwTo, pwChar, pwPw } =
    (pwPw !? (pwFrom - 1) == Just pwChar) /= (pwPw !? (pwTo - 1) == Just pwChar)

parsePws :: String -> Either String [Pw]
parsePws = first show . parse (many pwP) ""

pwP :: Parser Pw
pwP = do
    pwFrom      <- readDigit =<< label "from" (some digitChar)
    (_ :: Char) <- char '-'
    pwTo        <- readDigit =<< label "to" (some digitChar)
    space
    pwChar      <- label "pwChar" letterChar
    (_ :: Char) <- char ':'
    space
    pwPw <- Vec.fromList <$> manyTill letterChar eol
    pure $ Pw { pwFrom, pwTo, pwChar, pwPw }
  where
    readDigit :: String -> Parser Int
    readDigit = maybe (fail "not a digit") pure . readMaybe

type Parser = Parsec Void String
