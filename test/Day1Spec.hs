module Day1Spec where

import Day1
import Test.Tasty.HUnit

unit_Day1_computeTask1 :: Assertion
unit_Day1_computeTask1 = computeTask1 parsedInput @?= 1013211

unit_Day1_computeTask2 :: Assertion
unit_Day1_computeTask2 = computeTask2 parsedInput @?= 13891280
