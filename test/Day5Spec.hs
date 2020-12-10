module Day5Spec where

import Test.Tasty.HUnit
import Day5

unit_Day5_computeTask1 :: Assertion
unit_Day5_computeTask1 = computeTask1 parsedInput @?= 904

unit_Day5_computeTask2 :: Assertion
unit_Day5_computeTask2 = computeTask2 parsedInput @?= 669
