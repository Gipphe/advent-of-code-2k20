module Day4Spec where

import Day4
import Test.Tasty.HUnit

unit_Day4_computeTask1 :: Assertion
unit_Day4_computeTask1 = computeTask1 parsedInput @?= 196

unit_Day4_computeTask2 :: Assertion
unit_Day4_computeTask2 = computeTask2 parsedInput @?= 114
