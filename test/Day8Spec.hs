module Day8Spec where

import Test.Tasty.HUnit
import Day8

unit_Day8_computeTask1 :: Assertion
unit_Day8_computeTask1 = computeTask1 parsedInput @?= 2034

unit_Day8_computeTask2 :: Assertion
unit_Day8_computeTask2 = computeTask2 parsedInput @?= 672
