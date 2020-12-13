{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Day12 where

import Data.Foldable (foldl')
import Data.FileEmbed (embedStringFile)

import Util (SomeDay(..), Day, Task, runTask)

someDay12 :: SomeDay
someDay12 = SomeDay day12

day12 :: Day 12 ()
day12 = do
    runTask day12Task1
    runTask day12Task2

parseInput :: String -> [Action]
parseInput = fmap parseAction . lines

rawInput :: String
rawInput = $(embedStringFile "input/day12.txt")

parsedInput :: [Action]
parsedInput = parseInput rawInput

day12Task1 :: Task 1 Int
day12Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [Action] -> Int
computeTask1 = manhattanDistance . boatPos . foldl'
    (flip applyActionToBoat)
    (Boat East mempty)

day12Task2 :: Task 2 Int
day12Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [Action] -> Int
computeTask2 = manhattanDistance . waypointBoatPos . foldl'
    (flip applyActionToWaypoint)
    (Waypoint (Pos 10 1) mempty)

applyActionToWaypoint :: Action -> Waypoint -> Waypoint
applyActionToWaypoint action (Waypoint wp bp) = case action of
    MoveDir movedir steps -> Waypoint (wp <> moveInDir movedir steps) bp
    Forward steps         -> Waypoint wp (bp <> multiplyPos steps wp)
    Turn turnDir deg      -> Waypoint (turnWaypoint turnDir deg wp) bp

turnWaypoint :: Turn -> Int -> Pos -> Pos
turnWaypoint turnDir deg wp = case turnDir of
    TurnL -> times numTurns rotL wp
    TurnR -> times numTurns rotR wp
  where
    times 0 _ x = x
    times n f x = times (n - 1) f (f x)
    rotL (Pos x y) = Pos (-y) x
    rotR (Pos x y) = Pos y (-x)
    numTurns = (deg `div` 90) `mod` 4

multiplyPos :: Int -> Pos -> Pos
multiplyPos n (Pos x y) = Pos (x * n) (y * n)

manhattanDistance :: Pos -> Int
manhattanDistance (Pos x y) = abs x + abs y

applyActionToBoat :: Action -> Boat -> Boat
applyActionToBoat action (Boat dir pos) = case action of
    MoveDir movedir steps -> Boat dir (pos <> moveInDir movedir steps)
    Forward steps         -> Boat dir (pos <> moveInDir dir steps)
    Turn turnDir deg      -> Boat (turn turnDir deg dir) pos

turn :: Turn -> Int -> Direction -> Direction
turn t deg dir = case t of
    TurnL -> enumDown steps dir
    TurnR -> enumUp steps dir
    where steps = deg `div` 90

moveInDir :: Direction -> Int -> Pos
moveInDir dir len = case dir of
    North -> Pos 0 len
    East  -> Pos len 0
    South -> Pos 0 (-len)
    West  -> Pos (-len) 0

enumUp :: (Enum a, Bounded a, Eq a) => Int -> a -> a
enumUp 0 x = x
enumUp n x
    | n < 0         = error "enumUp: n less than 0"
    | x == maxBound = enumUp (n - 1) minBound
    | otherwise     = enumUp (n - 1) (succ x)

enumDown :: (Enum a, Bounded a, Eq a) => Int -> a -> a
enumDown 0 x = x
enumDown n x
    | n < 0         = error "enumDown: n less than 0"
    | x == minBound = enumDown (n - 1) maxBound
    | otherwise     = enumDown (n - 1) (pred x)

data Waypoint = Waypoint
    { waypointPos     :: Pos
    , waypointBoatPos :: Pos
    }

data Boat = Boat
    { boatDir :: Direction
    , boatPos :: Pos
    }
    deriving Show

data Pos = Pos
    { posX :: Int
    , posY :: Int
    }
    deriving (Eq, Ord, Show)

instance Semigroup Pos where
    (Pos x1 y1) <> (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)

instance Monoid Pos where
    mempty = Pos 0 0

data Turn = TurnL | TurnR
    deriving (Show)

data Direction
    = North
    | East
    | South
    | West
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Semigroup Direction where
    _ <> x = x

data Action
    = MoveDir Direction Int
    | Turn Turn Int
    | Forward Int
    deriving (Show)

parseAction :: String -> Action
parseAction = \case
    'N' : len -> MoveDir North $ read len
    'S' : len -> MoveDir South $ read len
    'E' : len -> MoveDir East $ read len
    'W' : len -> MoveDir West $ read len
    'L' : len -> Turn TurnL $ read len
    'R' : len -> Turn TurnR $ read len
    'F' : len -> Forward $ read len
    x         -> error $ "Unrecognized command: '" <> x <> "'"
