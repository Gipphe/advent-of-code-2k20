module Lib
    ( main
    ) where

import Data.List (intersperse)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Util

days :: [SomeDay]
days =
    [ someDay1
    , someDay2
    , someDay3
    , someDay4
    , someDay5
    , someDay6
    , someDay7
    , someDay8
    , someDay9
    , someDay10
    , someDay11
    , someDay12
    , someDay13
    , someDay14
    , someDay15
    ]

main :: IO ()
main = do
    startTime <- getPOSIXTime
    sequence_ $ intersperse (putStrLn sep) $ runSomeDay <$> days
    endTime <- getPOSIXTime
    putStrLn
        $  sep
        <> "\nTook "
        <> show (endTime - startTime)
        <> " to run all days"
    where sep = "---------"
