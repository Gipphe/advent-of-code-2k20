module Day15Spec where

import Test.Tasty.HUnit
import Day15

unit_Day15_computeTask1 :: Assertion
unit_Day15_computeTask1 = computeTask1 parsedInput @?= 468

unit_Day15_computeTask2 :: Assertion
unit_Day15_computeTask2 = computeTask2 parsedInput @?= 1801753
