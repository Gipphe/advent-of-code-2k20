module Day15Spec where

import Day15
import Test.Tasty.HUnit

unit_Day15_computeTask1 :: Assertion
unit_Day15_computeTask1 = computeTask1 parsedInput @?= 468

unit_Day15_computeTask2 :: Assertion
unit_Day15_computeTask2 = computeTask2 parsedInput @?= 1801753
