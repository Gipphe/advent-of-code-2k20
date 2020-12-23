{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Control.Applicative ((<|>))
import Data.FileEmbed (embedStringFile)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
    ( Parsec
    , errorBundlePretty
    , notFollowedBy
    , parse
    , sepBy
    , sepEndBy
    , some
    , try
    )
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, string)

import Util (Day, SomeDay(..), Task, runTask, splitOnDoubleNewline)

someDay19 :: SomeDay
someDay19 = SomeDay day19

day19 :: Day 19 ()
day19 = do
    runTask day19Task1
    runTask day19Task2

day19Task1 :: Task 1 Int
day19Task1 = pure $ computeTask1 parsedInput

day19Task2 :: Task 2 Int
day19Task2 = pure $ computeTask2 parsedInput

computeTask1 :: Game -> Int
computeTask1 Game { rules, messages } =
    length . filter (matchesRule rulesMap 0) $ messages
    where rulesMap = M.fromList rules

computeTask2 :: Game -> Int
computeTask2 Game { rules, messages } =
    length . filter (matchesRule rulesMap 0) $ messages
  where
    rulesMap    = updateRules $ M.fromList rules
    updateRules = M.union $ M.fromList [rule8, rule11]
    rule8       = (8, Rule $ Or [Seq [42], Seq [42, 8]])
    rule11      = (11, Rule $ Or [Seq [42, 31], Seq [42, 11, 31]])

matchesRule :: IntMap Rule -> Int -> String -> Bool
matchesRule m i s = ("" `elem`) $ stripRule m i s

stripRule :: IntMap Rule -> Int -> String -> [String]
stripRule _ _     "" = []
stripRule m index s  = case rule of
    Lit  c        -> if head s == c then [tail s] else []
    Rule (Or ors) -> foldMap (stripRuleSeq s) ors
  where
    rule =
        fromMaybe (error $ "Unknown index: " <> show index) $ M.lookup index m
    stripRuleSeq :: String -> Seq -> [String]
    stripRuleSeq s' (Seq []       ) = [s']
    stripRuleSeq s' (Seq (i : ids)) = do
        stripped <- stripRule m i s'
        stripRuleSeq stripped (Seq ids)

data Rule
    = Rule Or
    | Lit Char
    deriving (Eq)

instance Show Rule where
    show = \case
        Rule ors -> show ors
        Lit  c   -> "\"" <> pure c <> "\""

newtype Or = Or [Seq]
    deriving (Eq)

instance Show Or where
    show (Or sqs) = "(" <> intercalate " | " (show <$> sqs) <> ")"

newtype Seq = Seq [Int]
    deriving (Eq)

instance Show Seq where
    show (Seq ids) = "(" <> intercalate " " (show <$> ids) <> ")"

data Game = Game
    { rules    :: [(Int, Rule)]
    , messages :: [String]
    }
    deriving Show

rawInput :: String
rawInput = $(embedStringFile "input/day19.txt")

parsedInput :: Game
parsedInput = parseGame rawInput

parseGame :: String -> Game
parseGame =
    either (error . errorBundlePretty) id . mkGame . splitOnDoubleNewline
  where
    mkGame [rs, msgs] = Game <$> parse rulesP "" rs <*> parse messagesP "" msgs
    mkGame xs = error $ "Did not get two parts for Game: " <> show (length xs)

rulesP :: Parser [(Int, Rule)]
rulesP = sepEndBy ruleWithIDP eol
  where
    ruleWithIDP = (,) <$> (read <$> some digitChar) <* string ": " <*> ruleP
    ruleP       = litP <|> (Rule <$> orP)
    litP        = Lit <$ char '"' <*> letterChar <* char '"'
    orP         = Or <$> sepBy seqP (string " | ")
    seqP = Seq <$> sepBy refP (try $ char ' ' <* notFollowedBy (char '|'))
    refP        = read <$> some digitChar

messagesP :: Parser [String]
messagesP = sepEndBy (some letterChar) eol

type Parser = Parsec Void String
