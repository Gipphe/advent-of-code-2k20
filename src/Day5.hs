{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Day5 where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (sort)
import Data.Void (Void)
import Text.Megaparsec (count, parse, sepBy, Parsec)
import Text.Megaparsec.Char (char, eol)

import Util (Day, SomeDay(..), Task, runTask, findExactlyOne, trim)

someDay5 :: SomeDay
someDay5 = SomeDay day5

day5 :: Day 5 ()
day5 = do
    runTask day5Task1
    runTask day5Task2

ioInput :: IO [Seat]
ioInput = either fail pure . parseSeats . trim =<< readFile "input/day5.txt"

day5Task1 :: Task 1 Int
day5Task1 = do
    input <- liftIO ioInput
    pure $ maximum $ getSeatID <$> input

day5Task2 :: Task 2 Int
day5Task2 = do
    input <- liftIO ioInput
    let seatIDs = sort $ getSeatID <$> input
    maybe (fail "Could not find seat") pure
        $ findExactlyOne
              (`notElem` seatIDs)
              [minimum seatIDs .. maximum seatIDs]

findAxis :: String -> Int
findAxis = \case
    x : xs | x == 'B' || x == 'R' -> (2 ^ length xs) + findAxis xs
    x : xs | x == 'F' || x == 'L' -> findAxis xs
    []                            -> 0
    x                             -> error $ "Unrecognized designator: " <> x

parseSeats :: String -> Either String [Seat]
parseSeats = first show . parse seatsP ""

seatsP :: Parser [Seat]
seatsP = sepBy seatP eol

seatP :: Parser Seat
seatP = do
    designation <- count 10 (char 'F' <|> char 'B' <|> char 'L' <|> char 'R')
    let row    = Row $ findAxis (take 7 designation)
        column = Column $ findAxis (drop 7 designation)
    pure $ Seat designation row column

getSeatID :: Seat -> Int
getSeatID (Seat _ (Row row) (Column column)) = row * 8 + column

data Seat = Seat
    { seatDesignation :: String
    , seatRow :: Row
    , seatColumn :: Column
    }

newtype Row = Row { getRow :: Int }

newtype Column = Column { getColumn :: Int }

type Parser = Parsec Void String

-- BFFFBBF
