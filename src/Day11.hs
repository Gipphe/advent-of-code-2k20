{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Day11 where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector ((!?), Vector)
import qualified Data.Vector as Vec
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, parse, sepEndBy, some)
import Text.Megaparsec.Char (char, eol)

import Util (Day, SomeDay(..), Task, runTask)

someDay11 :: SomeDay
someDay11 = SomeDay day11

day11 :: Day 11 ()
day11 = do
    runTask day11Task1
    runTask day11Task2

parseInput :: String -> Vector (Vector Seat)
parseInput = either error id . parseSeats

rawInput :: String
rawInput = $(embedStringFile "input/day11.txt")

parsedInput :: Vector (Vector Seat)
parsedInput = parseInput rawInput

day11Task1 :: Task 1 Int
day11Task1 = pure $ computeTask1 parsedInput

computeTask1 :: Vector (Vector Seat) -> Int
computeTask1 previousState
    | previousState == newState = countSeats newState
    | otherwise                 = computeTask1 newState
    where newState = step iterateNeighbor previousState

iterateNeighbor :: Int -> Int -> Vector (Vector Seat) -> Seat -> Seat
iterateNeighbor x y xs = \case
    EmptySeat | occupiedSeats == 0 -> OccupiedSeat
    OccupiedSeat | occupiedSeats >= 4 -> EmptySeat
    s                              -> s
  where
    adjacent =
        [ (x', y')
        | y' <- [(y - 1) .. (y + 1)]
        , x' <- [(x - 1) .. (x + 1)]
        , not (x' == x && y' == y)
        , x' >= 0 && x' < maybe 0 Vec.length (xs !? 0)
        , y' >= 0 && y' < Vec.length xs
        ]
    occupiedSeats = sumOccupiedSeat neighbors
    neighbors     = uncurry lookupNeighbor <$> adjacent
    lookupNeighbor x'' y'' = fromMaybe NoSeat $ (!? x'') =<< xs !? y''

day11Task2 :: Task 2 Int
day11Task2 = pure $ computeTask2 parsedInput

computeTask2 :: Vector (Vector Seat) -> Int
computeTask2 previousState
    | previousState == newState = countSeats newState
    | otherwise                 = computeTask2 newState
    where newState = step iterateDistantNeighbor previousState

iterateDistantNeighbor :: Int -> Int -> Vector (Vector Seat) -> Seat -> Seat
iterateDistantNeighbor x y xs = \case
    EmptySeat | occupiedSeats == 0 -> OccupiedSeat
    OccupiedSeat | occupiedSeats >= 5 -> EmptySeat
    s                              -> s
  where
    occupiedSeats            = sumOccupiedSeat firstSeatInEachDirection
    firstSeatInEachDirection = mapMaybe firstActualSeat seatsInEachDirection
    seatsInEachDirection =
        (\(stepX, stepY) -> diagonalSeats stepX stepY x y xs) <$> vectors
    vectors =
        [ (x', y')
        | let is = [-1 .. 1]
        , x' <- is
        , y' <- is
        , x' /= 0 || y' /= 0
        ]
    firstActualSeat []             = Nothing
    firstActualSeat (NoSeat : xs') = firstActualSeat xs'
    firstActualSeat (x'     : _  ) = Just x'

sumOccupiedSeat :: [Seat] -> Int
sumOccupiedSeat = length . filter (== OccupiedSeat)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

diagonalSeats :: Int -> Int -> Int -> Int -> Vector (Vector Seat) -> [Seat]
diagonalSeats stepX stepY x y xs = mapUntilFirstNothing
    (\(x', y') -> lookupSeat x' y' xs)
    coords
  where
    ys'    = iterate (+ stepY) (y + stepY)
    xs'    = iterate (+ stepX) (x + stepX)
    coords = zip xs' ys'

mapUntilFirstNothing :: (a -> Maybe b) -> [a] -> [b]
mapUntilFirstNothing f = \case
    []       -> []
    (x : xs) -> case f x of
        Nothing -> []
        Just y  -> y : mapUntilFirstNothing f xs

countSeats :: Vector (Vector Seat) -> Int
countSeats = length . Vec.filter (== OccupiedSeat) . join

lookupSeat :: Int -> Int -> Vector (Vector a) -> Maybe a
lookupSeat x y xs = (!? x) =<< (!? y) =<< pure xs

step :: (Int -> Int -> Vector (Vector Seat) -> Seat -> Seat)
     -> Vector (Vector Seat)
     -> Vector (Vector Seat)
step iterateSeat previousState = Vec.ifoldr
    (\y ->
        Vec.cons
            . Vec.ifoldr (\x -> Vec.cons . iterateSeat x y previousState) mempty
    )
    mempty
    previousState

parseSeats :: String -> Either String (Vector (Vector Seat))
parseSeats = first show . parse seatsP ""

data Seat
    = NoSeat
    | EmptySeat
    | OccupiedSeat
    deriving (Eq)

instance Show Seat where
    show = \case
        NoSeat       -> "N"
        EmptySeat    -> "E"
        OccupiedSeat -> "O"

seatsP :: Parser (Vector (Vector Seat))
seatsP = Vec.fromList <$> sepEndBy seatRowP eol

seatRowP :: Parser (Vector Seat)
seatRowP = Vec.fromList <$> some seatP

seatP :: Parser Seat
seatP = noSeatP <|> emptySeatP <|> takenSeatP

noSeatP :: Parser Seat
noSeatP = char '.' $> NoSeat

emptySeatP :: Parser Seat
emptySeatP = char 'L' $> EmptySeat

takenSeatP :: Parser Seat
takenSeatP = char '#' $> OccupiedSeat

adjustV :: Int -> (a -> a) -> Vector a -> Vector a
adjustV i f = Vec.imap mapFn
  where
    mapFn i' x
        | i' == i   = f x
        | otherwise = x

type Parser = Parsec Void String

traceShowLabelId :: Show a => String -> a -> a
traceShowLabelId label a = trace (label <> show a) a
