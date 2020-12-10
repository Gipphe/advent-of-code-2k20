module Day6Spec where

import Test.Tasty.HUnit
import Day6

unit_Day6_computeTask1 :: Assertion
unit_Day6_computeTask1 = computeTask1 parsedInput @?= 6161

unit_Day6_computeTask2 :: Assertion
unit_Day6_computeTask2 = computeTask2 parsedInput @?= 2971
