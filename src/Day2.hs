{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day2
    ( day2
    , day2Task1
    , day2Task2
    , someDay2
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (parse, many, some, manyTill, label, Parsec)
import Text.Megaparsec.Char (char, digitChar, space, letterChar, eol)
import Text.Read (readMaybe)

import Util (Day, Task, SomeDay(..), runTask, safeIndex, sumTrue)

someDay2 :: SomeDay
someDay2 = SomeDay day2

day2 :: Day 2 ()
day2 = do
    runTask day2Task1
    runTask day2Task2

data Pw = Pw
    { pwFrom :: Int
    , pwTo :: Int
    , pwChar :: Char
    , pwPw :: String
    }

ioInput :: IO [Pw]
ioInput = either fail pure . parsePws =<< readFile "input/day2.txt"

day2Task1 :: Task 1 Int
day2Task1 = do
    sumTrue . fmap validatePw <$> liftIO ioInput

day2Task2 :: Task 2 Int
day2Task2 = do
    sumTrue . fmap validateWithRealPolicy <$> liftIO ioInput

validatePw :: Pw -> Bool
validatePw Pw { pwFrom, pwTo, pwChar, pwPw } = isWithinLimits
    $ foldr countChar 0 pwPw
  where
    countChar c = if c == pwChar then (+ 1) else id
    isWithinLimits x = x >= pwFrom && x <= pwTo

validateWithRealPolicy :: Pw -> Bool
validateWithRealPolicy Pw { pwFrom, pwTo, pwChar, pwPw } =
    (safeIndex (pwFrom - 1) pwPw == Just pwChar)
        /= (safeIndex (pwTo - 1) pwPw == Just pwChar)

parsePws :: String -> Either String [Pw]
parsePws = first show . parse (many pwP) ""

pwP :: Parser Pw
pwP = do
    pwFrom      <- readDigit =<< label "from" (some digitChar)
    (_ :: Char) <- char '-'
    pwTo        <- readDigit =<< label "to" (some digitChar)
    space
    pwChar      <- label "pwChar" letterChar
    (_ :: Char) <- char ':'
    space
    pwPw <- manyTill letterChar eol
    pure $ Pw { pwFrom, pwTo, pwChar, pwPw }
  where
    readDigit :: String -> Parser Int
    readDigit = maybe (fail "not a digit") pure . readMaybe

type Parser = Parsec Void String
