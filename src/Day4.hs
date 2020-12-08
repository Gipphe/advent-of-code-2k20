{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Day4 where

import Control.Applicative (Alternative, (<|>))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy, some, choice)
import Text.Megaparsec.Char (char, eol, alphaNumChar, string)
import Text.Read (readMaybe)

import Util (SomeDay(..), Day, Task, runTask, splitOnDoubleNewline)

data Passport m = Passport
    { birthYear      :: m String
    , issueYear      :: m String
    , expirationYear :: m String
    , height         :: m String
    , hairColor      :: m String
    , eyeColor       :: m String
    , passportId     :: m String
    , countryId      :: Maybe String
    }

instance (Alternative m) => Semigroup (Passport m) where
    p1 <> p2 = Passport
        { birthYear      = birthYear p2 <|> birthYear p1
        , issueYear      = issueYear p2 <|> issueYear p1
        , expirationYear = expirationYear p2 <|> expirationYear p1
        , height         = height p2 <|> height p1
        , hairColor      = hairColor p2 <|> hairColor p1
        , eyeColor       = eyeColor p2 <|> eyeColor p1
        , passportId     = passportId p2 <|> passportId p1
        , countryId      = countryId p2 <|> countryId p1
        }

instance (Alternative m, Monoid (m String)) => Monoid (Passport m) where
    mempty = Passport mempty mempty mempty mempty mempty mempty mempty mempty

deriving instance (Show (m String)) => Show (Passport m)

validatePassport :: Passport Maybe -> Maybe (Passport Identity)
validatePassport p = do
    birthYear      <- Identity <$> birthYear p
    issueYear      <- Identity <$> issueYear p
    expirationYear <- Identity <$> expirationYear p
    height         <- Identity <$> height p
    hairColor      <- Identity <$> hairColor p
    eyeColor       <- Identity <$> eyeColor p
    passportId     <- Identity <$> passportId p
    countryId      <- pure $ countryId p
    pure $ Passport { .. }

validateHarder :: Passport Identity -> Maybe (Passport Identity)
validateHarder p = do
    birthYear      <- traverse validateByr (birthYear p)
    issueYear      <- traverse validateIyr (issueYear p)
    expirationYear <- traverse validateEyr (expirationYear p)
    height         <- traverse validateHgt (height p)
    hairColor      <- traverse validateHcl (hairColor p)
    eyeColor       <- traverse validateEcl (eyeColor p)
    passportId     <- traverse validatePid (passportId p)
    countryId      <- pure $ countryId p
    pure $ Passport { .. }
  where
    validateByr = isBetween 1920 2002
    validateIyr = isBetween 2010 2020
    validateEyr = isBetween 2020 2030
    validateHgt =
        (\case
                (digits, "cm") -> isBetween 150 193 digits
                (digits, "in") -> isBetween 59 76 digits
                _              -> Nothing
            )
            . span isDigit
    validateHcl = \case
        ss@('#' : rest) ->
            flip ifTrue ss . const $ all (\c -> isDigit c || isAlphaHex c) rest
        _ -> Nothing
        where isAlphaHex c = (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    validateEcl x
        | x `elem` validColors = Just x
        | otherwise            = Nothing
        where validColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    validatePid x
        | length x == 9 = Just x
        | otherwise     = Nothing
    ifTrue :: (a -> Bool) -> a -> Maybe a
    ifTrue p' x
        | p' x      = Just x
        | otherwise = Nothing
    isBetween :: Int -> Int -> String -> Maybe String
    isBetween low high =
        fmap show . ifTrue (\x -> x >= low && x <= high) <=< readMaybe

data Field = Field Tag String
    deriving Show

data Tag
    = BYR
    | IYR
    | EYR
    | HGT
    | HCL
    | ECL
    | PID
    | CID
    deriving (Show)

someDay4 :: SomeDay
someDay4 = SomeDay day4

day4 :: Day 4 ()
day4 = do
    runTask day4Task1
    runTask day4Task2

ioInput :: IO [Passport Maybe]
ioInput =
    either fail pure
        .   parsePassports
        .   intercalate "\n"
        .   fmap (unwords . lines)
        .   splitOnDoubleNewline
        =<< readFile "input/day4.txt"

day4Task1 :: Task 1 Int
day4Task1 = do
    input <- liftIO ioInput
    pure $ length $ mapMaybe validatePassport input

day4Task2 :: Task 2 Int
day4Task2 = do
    input <- liftIO ioInput
    pure $ length $ mapMaybe (validateHarder <=< validatePassport) input

parsePassports :: String -> Either String [Passport Maybe]
parsePassports = first show . parse passportsP ""

passportsP :: Parser [Passport Maybe]
passportsP = sepBy passportP eol

passportP :: Parser (Passport Maybe)
passportP = foldr (flip (<>) . initField) mempty <$> fieldsP

initField :: Field -> Passport Maybe
initField (Field tag val) = case tag of
    BYR -> mempty { birthYear = Just val }
    IYR -> mempty { issueYear = Just val }
    EYR -> mempty { expirationYear = Just val }
    HGT -> mempty { height = Just val }
    HCL -> mempty { hairColor = Just val }
    ECL -> mempty { eyeColor = Just val }
    PID -> mempty { passportId = Just val }
    CID -> mempty { countryId = Just val }

fieldsP :: Parser [Field]
fieldsP = sepBy fieldP (char ' ')

fieldP :: Parser Field
fieldP = Field <$> tagP <* char ':' <*> fieldValP

fieldValP :: Parser String
fieldValP = some $ alphaNumChar <|> char '#'

tagP :: Parser Tag
tagP = choice
    [ string "byr" $> BYR
    , string "iyr" $> IYR
    , string "eyr" $> EYR
    , string "hgt" $> HGT
    , string "hcl" $> HCL
    , string "ecl" $> ECL
    , string "pid" $> PID
    , string "cid" $> CID
    ]

type Parser = Parsec Void String
