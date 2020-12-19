{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Day18 where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.FileEmbed (embedStringFile)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepEndBy)
import Text.Megaparsec.Char (char, digitChar, eol)

import Util (Day, SomeDay(..), Task, chainl1, runTask)


----------------
-- * Boilerplate
----------------

someDay18 :: SomeDay
someDay18 = SomeDay day18

day18 :: Day 18 ()
day18 = do
    runTask day18Task1
    runTask day18Task2

day18Task1 :: Task 1 Int
day18Task1 = pure $ computeTask1 parsedInput

day18Task2 :: Task 2 Int
day18Task2 = pure $ computeTask2 parsedInput


---------
-- * Task
---------

computeTask1 :: (Num a, Read a) => String -> a
computeTask1 = sum . fmap evalExpr . parseInput

computeTask2 :: (Num a, Read a) => String -> a
computeTask2 = sum . fmap evalExpr . parseInput2

data Expr a
    = Sum !(Expr a) !(Expr a)
    | Prod !(Expr a) !(Expr a)
    | Lit !a

evalExpr :: Num a => Expr a -> a
evalExpr (Lit x   ) = x
evalExpr (Sum  x y) = evalExpr x + evalExpr y
evalExpr (Prod x y) = evalExpr x * evalExpr y


----------
-- * Input
----------

parseExprs :: Read a => String -> Either String [Expr a]
parseExprs = first errorBundlePretty . parse exprsP ""
  where
    exprsP = sepEndBy exprP eol
    exprP  = chainl1 term binOp
    term   = litP <|> parenP exprP
    binOp  = addOp <|> mulOp

parseExprsP2 :: Read a => String -> Either String [Expr a]
parseExprsP2 = first errorBundlePretty . parse exprsP2 ""
  where
    exprsP2 = sepEndBy exprP2 eol
    exprP2  = chainl1 factor mulOp
    factor  = chainl1 term addOp
    term    = litP <|> parenP exprP2

rawInput :: String
rawInput = $(embedStringFile "input/day18.txt")

parseInput :: Read a => String -> [Expr a]
parseInput = either error id . parseExprs . filter (/= ' ')

parseInput2 :: Read a => String -> [Expr a]
parseInput2 = either error id . parseExprsP2 . filter (/= ' ')

parsedInput :: String
parsedInput = rawInput

addOp :: Parser (Expr a -> Expr a -> Expr a)
addOp = Sum <$ char '+'

mulOp :: Parser (Expr a -> Expr a -> Expr a)
mulOp = Prod <$ char '*'

litP :: Read a => Parser (Expr a)
litP = Lit . read . pure <$> digitChar

parenP :: Parser a -> Parser a
parenP p = char '(' *> p <* char ')'

type Parser = Parsec Void String
