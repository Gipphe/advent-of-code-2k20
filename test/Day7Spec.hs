module Day7Spec where

import Day7
import Test.Tasty.HUnit

unit_Day7_computeTask1 :: Assertion
unit_Day7_computeTask1 = computeTask1 parsedInput @?= 144

unit_Day7_computeTask2 :: Assertion
unit_Day7_computeTask2 = computeTask2 parsedInput @?= 5956
