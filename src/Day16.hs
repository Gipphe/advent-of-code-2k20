{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Day16 where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List ((\\), isPrefixOf, sortOn, transpose)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec
    (Parsec, errorBundlePretty, parse, sepBy, sepEndBy, some, someTill)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, string)

import Util (Day, SomeDay(..), Task, runTask, splitOnDoubleNewline)


----------------
-- * Boilerplate
----------------

someDay16 :: SomeDay
someDay16 = SomeDay day16

day16 :: Day 16 ()
day16 = do
    runTask day16Task1
    runTask day16Task2

day16Task1 :: Task 1 Int
day16Task1 = pure $ computeTask1 parsedInput

day16Task2 :: Task 2 Int
day16Task2 = pure $ computeTask2 parsedInput


----------
-- * Tasks
----------

computeTask1 :: Game -> Int
computeTask1 Game { rules, otherTickets } =
    sum . filter (invalidBy rules) $ mconcat otherTickets

computeTask2 :: Game -> Int
computeTask2 Game { rules, otherTickets, ticket } =
    foldr ((\i acc -> (ticket !! i) * acc) . fst) 1
        . filter (("departure" `isPrefixOf`) . ruleName . snd)
        . matchRulesToColumns
        . allPossibleColumnsForRules rules
        . zip [0 ..]
        . transpose
        . (ticket :)
        $ removeInvalid rules otherTickets

allPossibleColumnsForRules :: [Rule] -> [(Int, Ticket)] -> Map Rule [Int]
allPossibleColumnsForRules rs = foldr f mempty
  where
    f :: (Int, Ticket) -> Map Rule [Int] -> Map Rule [Int]
    f (i, col) m = foldr (\r m' -> M.insertWith (++) r [i] m') m
        $ filter (\r -> all (satisfiesRule r) col) rs

matchRulesToColumns :: Map Rule [Int] -> [(Int, Rule)]
matchRulesToColumns = foldl' f [] . sortOn (length . snd) . M.toList
  where
    f :: [(Int, Rule)] -> (Rule, [Int]) -> [(Int, Rule)]
    f xs (r, possibleIndices) = case possibleIndices \\ (fst <$> xs) of
        [i] -> (i, r) : xs
        _   -> error "Hopefully doesn't happen?"

satisfiesRule :: Rule -> Int -> Bool
satisfiesRule Rule { ruleRanges } x =
    foldr ((||) . (`isInRange` x)) False ruleRanges

removeInvalid :: [Rule] -> [Ticket] -> [Ticket]
removeInvalid rs = filter (not . any (invalidBy rs))

invalidBy :: [Rule] -> Int -> Bool
invalidBy rs x = not . any (`isInRange` x) $ allRanges
    where allRanges = concatMap ruleRanges rs

isInRange :: RuleRange -> Int -> Bool
isInRange RuleRange { rangeFrom, rangeTo } x = rangeFrom <= x && x <= rangeTo


----------
-- * Input
----------

parseInput :: String -> Game
parseInput = either error id . parseGame

rawInput :: String
rawInput = $(embedStringFile "input/day16.txt")

parsedInput :: Game
parsedInput = parseInput rawInput

parseGame :: String -> Either String Game
parseGame = intoGame . splitOnDoubleNewline
  where
    intoGame [rules, ticket, otherTickets] =
        Game
            <$> parseRules rules
            <*> parseTicket ticket
            <*> parseOtherTickets otherTickets
    intoGame _ = error "Didn't get 3 sections"

parseRules :: String -> Either String [Rule]
parseRules = first errorBundlePretty . parse rulesP ""

parseTicket :: String -> Either String Ticket
parseTicket =
    first errorBundlePretty . parse ticketP "" . unlines . tail . lines

parseOtherTickets :: String -> Either String [Ticket]
parseOtherTickets =
    first errorBundlePretty . parse ticketsP "" . unlines . tail . lines

rulesP :: Parser [Rule]
rulesP = sepBy ruleP eol

ruleP :: Parser Rule
ruleP = Rule <$> ruleNameP <*> sepBy ruleRangeP (string " or ")

ruleNameP :: Parser String
ruleNameP = someTill (letterChar <|> char ' ') (string ": ")

ruleRangeP :: Parser RuleRange
ruleRangeP = do
    from <- read <$> some digitChar
    _    <- char '-'
    to   <- read <$> some digitChar
    pure $ RuleRange from to

ticketsP :: Parser [Ticket]
ticketsP = sepEndBy ticketP eol

ticketP :: Parser Ticket
ticketP = sepBy (read <$> some digitChar) (char ',')

data Game = Game
    { rules        :: [Rule]
    , ticket       :: Ticket
    , otherTickets :: [Ticket]
    }

type AnnotatedTicket = [(String, Int)]

type TicketFieldName = String

type PotentialAnnotatedTicket = [([TicketFieldName], Int)]

type Ticket = [TicketFieldValue]

type TicketFieldValue = Int

type RuleName = String

data Rule = Rule
    { ruleName   :: RuleName
    , ruleRanges :: [RuleRange]
    }

instance Eq Rule where
    (==) = (==) `on` ruleName

instance Ord Rule where
    compare = compare `on` ruleName

data RuleRange = RuleRange
    { rangeFrom :: Int
    , rangeTo   :: Int
    }

type Parser = Parsec Void String

matchesAny :: (Monoid m, Foldable t) => t (a -> m) -> a -> m
matchesAny fs x = foldl' (\acc f -> acc <> f x) mempty fs
