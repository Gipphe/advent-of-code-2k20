module Day9Spec where

import Test.Tasty.HUnit
import Day9

unit_Day9_computeTask1 :: Assertion
unit_Day9_computeTask1 = computeTask1 parsedInput @?= 20874512

unit_Day9_computeTask2 :: Assertion
unit_Day9_computeTask2 = computeTask2 parsedInput @?= 3012420
