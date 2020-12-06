module Lib
    ( main
    )
where

import Data.List (intersperse)

import Day1
import Day2
import Day3
import Util

days :: [SomeDay]
days = [someDay1, someDay2, someDay3]

main :: IO ()
main = sequence_ $ intersperse (putStrLn "---------") $ runSomeDay <$> days
