module Day10Spec where

import Test.Tasty.HUnit
import Day10

unit_Day10_computeTask1 :: Assertion
unit_Day10_computeTask1 = computeTask1 parsedInput @?= 2775

unit_Day10_computeTask2 :: Assertion
unit_Day10_computeTask2 = computeTask2 parsedInput @?= 518344341716992
