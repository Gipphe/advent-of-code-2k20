module Day9Spec where

import Day9
import Test.Tasty.HUnit

unit_Day9_computeTask1 :: Assertion
unit_Day9_computeTask1 = computeTask1 parsedInput @?= 20874512

unit_Day9_computeTask2 :: Assertion
unit_Day9_computeTask2 = computeTask2 parsedInput @?= 3012420
