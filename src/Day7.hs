{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Day7 where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Functor (($>))
import Data.Hashable (hash)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (Parsec, optional, parse, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, string)
import Text.Read (readMaybe)

import Util (Day, SomeDay(..), Task, runTask)

someDay7 :: SomeDay
someDay7 = SomeDay day7

day7 :: Day 7 ()
day7 = do
    runTask day7Task1
    runTask day7Task2

parseInput :: String -> BagRules
parseInput = either error id . parseBagRules

rawInput :: String
rawInput = $(embedStringFile "input/day7.txt")

parsedInput :: BagRules
parsedInput = parseInput rawInput

day7Task1 :: Task 1 Int
day7Task1 = pure $ computeTask1 parsedInput

computeTask1 :: BagRules -> Int
computeTask1 input =
    length
        . filter omitShinyGold
        . filter (canHaveShinyGoldBag input)
        . M.toList
        $ input
  where
    omitShinyGold (!b, _) | b == shinyGoldBag = False
    omitShinyGold _                           = True

day7Task2 :: Task 2 Int
day7Task2 = pure $ computeTask2 parsedInput

computeTask2 :: BagRules -> Int
computeTask2 input =
    maybe (error "No shiny gold bag rule") (sumBags input)
        $ M.lookup shinyGoldBag input

sumBags :: BagRules -> Set BagQuantity -> Int
sumBags bagRules bagQuantities
    | S.null bagQuantities = 0
    | otherwise = sum $ sumBagQuantity bagRules <$> S.toList bagQuantities

sumBagQuantity :: BagRules -> BagQuantity -> Int
sumBagQuantity bagRules (BagQuantity !n !bag) =
    maybe 0 ((n *) . (+ 1) . sumBags bagRules) $ M.lookup bag bagRules

canHaveShinyGoldBag :: BagRules -> BagRule -> Bool
canHaveShinyGoldBag bagRules = \case
    (b, _) | b == shinyGoldBag -> True
    (_, bagQuantities) ->
        let
            bagsWithin     = S.map bagQuantityBag bagQuantities
            bagRulesWithin = M.restrictKeys bagRules bagsWithin
        in any (canHaveShinyGoldBag bagRules) $ M.toList bagRulesWithin

type BagRules = Map Bag (Set BagQuantity)

type BagRule = (Bag, Set BagQuantity)

data BagQuantity = BagQuantity
    { bagQuantityQuantity :: Int
    , bagQuantityBag      :: Bag
    }
    deriving (Eq, Ord, Show)

data Bag = Bag Adjective Color
    deriving (Eq, Ord, Show)

type Adjective = Int
type Color = Int

shinyGoldBag :: Bag
shinyGoldBag = Bag (hash "shiny") (hash "gold")

parseBagRules :: String -> Either String BagRules
parseBagRules = first show . parse bagRulesP ""

bagRulesP :: Parser BagRules
bagRulesP = M.fromList <$> sepEndBy bagRuleP eol

bagRuleP :: Parser BagRule
bagRuleP = (,) <$> bagP <* string " contain " <*> bagQuantitiesP

bagQuantitiesP :: Parser (Set BagQuantity)
bagQuantitiesP =
    fmap S.fromList
        $  ((string "no other bags" $> []) <|> sepBy bagQuantityP (string ", "))
        <* char '.'

bagQuantityP :: Parser BagQuantity
bagQuantityP =
    BagQuantity
        <$> (   maybe (fail "Not a valid number") pure
            .   readMaybe
            =<< some digitChar
            )
        <*  char ' '
        <*> bagP

bagP :: Parser Bag
bagP = Bag <$> adjectiveP <* char ' ' <*> colorP <* string " bag" <* optional
    (char 's')

adjectiveP :: Parser Adjective
adjectiveP = hash <$> some letterChar

colorP :: Parser Color
colorP = hash <$> some letterChar

type Parser = Parsec Void String
