module Day3Spec where

import Day3
import Test.Tasty.HUnit

unit_Day3_computeTask1 :: Assertion
unit_Day3_computeTask1 = computeTask1 parsedInput @?= 292

unit_Day3_computeTask2 :: Assertion
unit_Day3_computeTask2 = computeTask2 parsedInput @?= 9354744432
