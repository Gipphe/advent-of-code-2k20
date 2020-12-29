{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Day20 where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bool (bool)
import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Data.Vector ((!?), Vector)
import qualified Data.Vector as Vec
import Data.Void (Void)
import Debug.Trace (trace, traceShowId)
import Text.Megaparsec
    ( Parsec
    , eof
    , errorBundlePretty
    , optional
    , parse
    , sepBy
    , sepBy1
    , some
    , someTill
    , try
    )
import Text.Megaparsec.Char (char, digitChar, eol, string)
import Text.Megaparsec.Debug (dbg)

import Util (Day, SomeDay(..), Task, runTask, splitOnDoubleNewline)

someDay20 :: SomeDay
someDay20 = SomeDay day20

day20 :: Day 20 ()
day20 = do
    runTask day20Task1
    -- runTask day20Task2

day20Task1 :: Task 1 Int
day20Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [MapPiece] -> Int
computeTask1 pieces = length $ traceShowId pieces

type MapCell = Bool

type Map = Vector (Vector MapCell)

data MapPiece = MapPiece
    { mapPieceID  :: Int
    , mapPieceMap :: Map
    }

instance Show MapPiece where
    show (MapPiece i m) = "\nID: " <> show i <> "\n" <> showMap m
      where
        showMap     = intercalate "\n" . fmap showMapLine . Vec.toList
        showMapLine = fmap (bool '#' '.') . Vec.toList

isNextPiece :: MapPiece -> MapPiece -> Bool
isNextPiece m1 m2 = (hasSameEdge `on` mapPieceMap) m1 m2
  where
    hasSameEdge :: Map -> Map -> Bool
    hasSameEdge xs ys =
      where
        iterations :: [Map -> Map]
        iterations =
            (.)
                <$> [id, flipX, flipY]
                <*> [id, rotate, rotate . rotate, rotate . rotate . rotate]

    rightEdge = fmap Vec.last
    leftEdge  = fmap Vec.head
    flipX     = fmap Vec.reverse
    flipY     = Vec.reverse
    rotate =
        Vec.fromList
            . fmap (Vec.fromList . reverse)
            . transpose
            . fmap Vec.toList
            . Vec.toList

parsedInput :: [MapPiece]
parsedInput = parseInput rawInput

parseInput :: String -> [MapPiece]
parseInput =
    either (error . errorBundlePretty) id
        . traverse (parse mapPieceP "")
        . splitOnDoubleNewline

mapPiecesP :: Parser [MapPiece]
mapPiecesP = sepBy1 mapPieceP (try $ eol *> eol)

mapPieceP :: Parser MapPiece
mapPieceP = MapPiece <$> mapTitleP <* eol <*> mapPieceMapP
  where
    mapTitleP    = read <$ string "Tile " <*> some digitChar <* char ':'
    mapPieceMapP = Vec.fromList <$> some mapPieceLineP
    mapPieceLineP =
        Vec.fromList <$> some mapPixelP <* optional (void eol <|> eof)
    mapPixelP = fmap (== '#') $ char '#' <|> char '.'

rawInput :: String
rawInput = $(embedStringFile "input/day20.example.txt")

type Parser = Parsec Void String
