{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Day7 where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Functor (($>))
import Data.List (find)
import Data.Void (Void)
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

input :: [BagRule]
input = either undefined id $ parseBagRules $(embedStringFile "input/day7.txt")

day7Task1 :: Task 1 Int
day7Task1 = pure $ length $ filter omitShinyGold $ filter
    (canHaveShinyGoldBag input)
    input

day7Task2 :: Task 2 Int
day7Task2 = maybe (fail "No shiny gold bag rule") (pure . sumBags input)
    $ find ((== shinyGoldBag) . bagRuleBag) input

sumBags :: [BagRule] -> BagRule -> Int
sumBags _ (BagRule _ []) = 0
sumBags bagRules (BagRule _ bagQuantities) =
    sum $ sumBagQuantity bagRules <$> bagQuantities

sumBagQuantity :: [BagRule] -> BagQuantity -> Int
sumBagQuantity bagRules (BagQuantity n bag) =
    maybe 0 ((n *) . (+ 1) . sumBags bagRules)
        $ find ((== bag) . bagRuleBag) bagRules

omitShinyGold :: BagRule -> Bool
omitShinyGold (BagRule b _) | b == shinyGoldBag = False
omitShinyGold _ = True

canHaveShinyGoldBag :: [BagRule] -> BagRule -> Bool
canHaveShinyGoldBag bagRules = \case
    BagRule b _ | b == shinyGoldBag -> True
    BagRule _ bagQuantities ->
        let
            bagsWithin     = bagQuantityBag <$> bagQuantities
            bagRulesWithin = filter ((`elem` bagsWithin) . bagRuleBag) bagRules
        in any (canHaveShinyGoldBag bagRules) bagRulesWithin

data BagRule = BagRule
    { bagRuleBag :: Bag
    , bagRuleQuantities :: [BagQuantity]
    }
    deriving (Show)

data BagQuantity = BagQuantity
    { bagQuantityQuantity :: Int
    , bagQuantityBag :: Bag
    }
    deriving (Show)

data Bag = Bag Adjective Color
    deriving (Eq, Show)

shinyGoldBag :: Bag
shinyGoldBag = Bag "shiny" "gold"

type Adjective = String
type Color = String

parseBagRules :: String -> Either String [BagRule]
parseBagRules = first show . parse bagRulesP ""

bagRulesP :: Parser [BagRule]
bagRulesP = sepEndBy bagRuleP eol

bagRuleP :: Parser BagRule
bagRuleP = BagRule <$> bagP <* string " contain " <*> bagQuantitiesP

bagQuantitiesP :: Parser [BagQuantity]
bagQuantitiesP =
    ((string "no other bags" $> []) <|> sepBy bagQuantityP (string ", "))
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
