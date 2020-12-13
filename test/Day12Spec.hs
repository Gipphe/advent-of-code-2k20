module Day12Spec where

import Test.Tasty.HUnit
import Day12

unit_Day12_computeTask1 :: Assertion
unit_Day12_computeTask1 = computeTask1 parsedInput @?= 998

unit_Day12_computeTask2 :: Assertion
unit_Day12_computeTask2 = computeTask2 parsedInput @?= 71586
