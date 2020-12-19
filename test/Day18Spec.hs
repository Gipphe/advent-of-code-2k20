module Day18Spec where

import Day18
import Test.Tasty.HUnit

unit_Day18_computeTask1 :: Assertion
unit_Day18_computeTask1 = computeTask1 parsedInput @?= (800602729153 :: Int)

unit_Day18_computeTask2 :: Assertion
unit_Day18_computeTask2 = computeTask2 parsedInput @?= (92173009047076 :: Int)
