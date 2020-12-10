module Day4Spec where

import Test.Tasty.HUnit
import Day4

unit_Day4_computeTask1 :: Assertion
unit_Day4_computeTask1 = computeTask1 parsedInput @?= 196

unit_Day4_computeTask2 :: Assertion
unit_Day4_computeTask2 = computeTask2 parsedInput @?= 114
