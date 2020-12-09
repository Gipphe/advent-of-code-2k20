module Lib
    ( main
    )
where

import Data.List (intersperse)

import Day1
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
    ]

main :: IO ()
main = sequence_ $ intersperse (putStrLn "---------") $ runSomeDay <$> days
