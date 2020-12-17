module Day5Spec where

import Day5
import Test.Tasty.HUnit

unit_Day5_computeTask1 :: Assertion
unit_Day5_computeTask1 = computeTask1 parsedInput @?= 904

unit_Day5_computeTask2 :: Assertion
unit_Day5_computeTask2 = computeTask2 parsedInput @?= 669
