{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Day17 where

import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Data.Vector ((!?), Vector)
import qualified Data.Vector as V

import Util (Day, SomeDay(..), Task, runTask, uncurry3, uncurry4)


----------------
-- * Boilerplate
----------------

someDay17 :: SomeDay
someDay17 = SomeDay day17

day17 :: Day 17 ()
day17 = do
    runTask day17Task1
    runTask day17Task2

day17Task1 :: Task 1 Int
day17Task1 = pure $ computeTask1 parsedInput

day17Task2 :: Task 2 Int
day17Task2 = pure $ computeTask2 parsedInput


---------
-- * Task
---------

computeTask1 :: HCube -> Int
computeTask1 hcube = countLiveInHCube $ foldr
    (\_ hc -> growCube <$> cycleHCube hc)
    (growCube <$> hcube)
    [(1 :: Int) .. 6]

computeTask2 :: HCube -> Int
computeTask2 hcube = countLiveInHCube $ foldr
    (\_ hc -> growHCube $ cycleHCube hc)
    (growHCube hcube)
    [(1 :: Int) .. 6]

type HCube = Vector Cube

type Cube = Vector Plane

type Plane = Vector Row

type Row = Vector Bool

showCube :: Cube -> String
showCube = intercalate "\n\n" . fmap showPlane . V.toList

showPlane :: Plane -> String
showPlane = intercalate "\n" . fmap showRow . V.toList

showRow :: Row -> String
showRow = fmap showCell . V.toList

showCell :: Bool -> Char
showCell True  = '#'
showCell False = '.'

countLiveInHCube :: HCube -> Int
countLiveInHCube =
    length . filter id . mconcat . mconcat . mconcat . V.toList . fmap
        (V.toList . fmap (V.toList . fmap V.toList))

indexHCube :: HCube -> Int -> Int -> Int -> Int -> Bool
indexHCube hc w z y x = Just True == ((!? x) =<< (!? y) =<< (!? z) =<< hc !? w)

indexCube :: Cube -> Int -> Int -> Int -> Bool
indexCube v z y x = Just True == ((!? x) =<< (!? y) =<< v !? z)

growHCube :: HCube -> HCube
growHCube hcube = fmap growCube $ V.cons e $ V.snoc hcube e
  where
    e        = emptyCube cubeLen planeLen rowLen
    cubeLen  = length $ V.head hcube
    planeLen = length $ V.head $ V.head hcube
    rowLen   = length $ V.head $ V.head $ V.head hcube

growCube :: Cube -> Cube
growCube cube = fmap growPlane $ V.cons e $ V.snoc cube e
  where
    e        = emptyPlane planeLen rowLen
    planeLen = length $ V.head cube
    rowLen   = length $ V.head $ V.head cube

growPlane :: Plane -> Plane
growPlane xs = fmap growRow $ V.cons e $ V.snoc xs e
  where
    e      = emptyRow rowLen
    rowLen = length $ V.head xs

emptyCube :: Int -> Int -> Int -> Cube
emptyCube cubeLen planeLen rowLen =
    V.replicate cubeLen (emptyPlane planeLen rowLen)

emptyPlane :: Int -> Int -> Plane
emptyPlane planeLen rowLen = V.replicate planeLen (emptyRow rowLen)

emptyRow :: Int -> Row
emptyRow rowLen = V.replicate rowLen False

growRow :: Row -> Row
growRow = (`V.snoc` False) . V.cons False

cycleHCube :: HCube -> HCube
cycleHCube hcube = V.imap (cycleCube hcube) hcube

cycleCube :: HCube -> Int -> Cube -> Cube
cycleCube hcube w = V.imap (cyclePlane hcube w)

cyclePlane :: HCube -> Int -> Int -> Plane -> Plane
cyclePlane hcube w z = V.imap (cycleRow hcube w z)

cycleRow :: HCube -> Int -> Int -> Int -> Row -> Row
cycleRow hcube w z y = V.imap (cycleState hcube w z y)

cycleState :: HCube -> Int -> Int -> Int -> Int -> Bool -> Bool
cycleState hcube w z y x = \case
    True  -> liveNeighbors == 2 || liveNeighbors == 3
    False -> liveNeighbors == 3
    where liveNeighbors = countLiveHCubeNeighbors hcube w z y x

countLiveCubeNeighbors :: Cube -> Int -> Int -> Int -> Int
countLiveCubeNeighbors cube z y x =
    length $ filter id $ uncurry3 (indexCube cube) <$> cubeNeighborCoords z y x

countLiveHCubeNeighbors :: HCube -> Int -> Int -> Int -> Int -> Int
countLiveHCubeNeighbors hcube w z y x =
    length
        $   filter id
        $   uncurry4 (indexHCube hcube)
        <$> hCubeNeighborCoords w z y x

cubeNeighborCoords :: Int -> Int -> Int -> [(Int, Int, Int)]
cubeNeighborCoords z y x =
    [ (z + z', y + y', x + x')
    | let is = [-1 .. 1]
    , z' <- is
    , y' <- is
    , x' <- is
    , x' /= 0 || y' /= 0 || z' /= 0
    ]

hCubeNeighborCoords :: Int -> Int -> Int -> Int -> [(Int, Int, Int, Int)]
hCubeNeighborCoords w z y x =
    [ (w + w', z + z', y + y', x + x')
    | let is = [-1 .. 1]
    , w' <- is
    , z' <- is
    , y' <- is
    , x' <- is
    , x' /= 0 || y' /= 0 || z' /= 0 || w' /= 0
    ]

----------
-- * Input
----------

parseInput :: String -> HCube
parseInput s = mkHCube . pure . pure $ fmap isLiveCell <$> lines s
  where
    isLiveCell '#' = True
    isLiveCell _   = False

rawInput :: String
rawInput = $(embedStringFile "input/day17.txt")

parsedInput :: HCube
parsedInput = parseInput rawInput

mkHCube :: [[[[Bool]]]] -> HCube
mkHCube bHCube = V.generate (length bHCube) (mkCube . (bHCube !!))

mkCube :: [[[Bool]]] -> Cube
mkCube bCube = V.generate (length bCube) (mkPlane . (bCube !!))

mkPlane :: [[Bool]] -> Plane
mkPlane bPlane = V.generate (length bPlane) (mkRow . (bPlane !!))

mkRow :: [Bool] -> Row
mkRow = V.fromList
