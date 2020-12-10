module Day3Spec where

import Test.Tasty.HUnit
import Day3

unit_Day3_computeTask1 :: Assertion
unit_Day3_computeTask1 = computeTask1 parsedInput @?= 292

unit_Day3_computeTask2 :: Assertion
unit_Day3_computeTask2 = computeTask2 parsedInput @?= 9354744432
