module Day17Spec where

import Day17
import Test.Tasty.HUnit

unit_Day17_computeTask1 :: Assertion
unit_Day17_computeTask1 = computeTask1 parsedInput @?= 424

unit_Day17_computeTask2 :: Assertion
unit_Day17_computeTask2 = computeTask2 parsedInput @?= 2460
