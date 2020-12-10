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
import Data.List (find)
import Data.Void (Void)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec (parse, Parsec, some, sepBy, sepEndBy, optional)
import Text.Megaparsec.Char (letterChar, digitChar, eol, char, string)
import Text.Read (readMaybe)

import Util (SomeDay(..), Day, Task, runTask)

someDay7 :: SomeDay
someDay7 = SomeDay day7

day7 :: Day 7 ()
day7 = do
    runTask day7Task1
    runTask day7Task2

parseInput :: String -> [BagRule]
parseInput = either error id . parseBagRules

rawInput :: String
rawInput = $(embedStringFile "input/day7.txt")

parsedInput :: [BagRule]
parsedInput = parseInput rawInput

day7Task1 :: Task 1 Int
day7Task1 = pure $ computeTask1 parsedInput

computeTask1 :: [BagRule] -> Int
computeTask1 input =
    length . filter omitShinyGold . filter (canHaveShinyGoldBag input) $ input
  where
    omitShinyGold (BagRule b _) | b == shinyGoldBag = False
    omitShinyGold _ = True

day7Task2 :: Task 2 Int
day7Task2 = pure $ computeTask2 parsedInput

computeTask2 :: [BagRule] -> Int
computeTask2 input =
    maybe (error "No shiny gold bag rule") (sumBags input)
        $ find ((== shinyGoldBag) . bagRuleBag) input

sumBags :: [BagRule] -> BagRule -> Int
sumBags !bagRules (BagRule _ bagQuantities)
    | S.null bagQuantities = 0
    | otherwise = sum $ sumBagQuantity bagRules <$> S.toList bagQuantities

sumBagQuantity :: [BagRule] -> BagQuantity -> Int
sumBagQuantity !bagRules (BagQuantity n bag) =
    maybe 0 ((n *) . (+ 1) . sumBags bagRules)
        $ find ((== bag) . bagRuleBag) bagRules

canHaveShinyGoldBag :: [BagRule] -> BagRule -> Bool
canHaveShinyGoldBag !bagRules = \case
    BagRule b _ | b == shinyGoldBag -> True
    BagRule _ bagQuantities ->
        let
            bagsWithin = S.map bagQuantityBag bagQuantities
            bagRulesWithin =
                filter ((`S.member` bagsWithin) . bagRuleBag) bagRules
        in any (canHaveShinyGoldBag bagRules) bagRulesWithin

data BagRule = BagRule
    { bagRuleBag :: Bag
    , bagRuleQuantities :: Set BagQuantity
    }
    deriving (Show)

data BagQuantity = BagQuantity
    { bagQuantityQuantity :: Int
    , bagQuantityBag :: Bag
    }
    deriving (Eq, Ord, Show)

data Bag = Bag Adjective Color
    deriving (Eq, Ord, Show)

type Adjective = String
type Color = String

shinyGoldBag :: Bag
shinyGoldBag = Bag "shiny" "gold"

parseBagRules :: String -> Either String [BagRule]
parseBagRules = first show . parse bagRulesP ""

bagRulesP :: Parser [BagRule]
bagRulesP = sepEndBy bagRuleP eol

bagRuleP :: Parser BagRule
bagRuleP = BagRule <$> bagP <* string " contain " <*> bagQuantitiesP

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
adjectiveP = some letterChar

colorP :: Parser Color
colorP = some letterChar

type Parser = Parsec Void String
