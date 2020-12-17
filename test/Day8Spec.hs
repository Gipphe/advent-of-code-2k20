module Day8Spec where

import Day8
import Test.Tasty.HUnit

unit_Day8_computeTask1 :: Assertion
unit_Day8_computeTask1 = computeTask1 parsedInput @?= 2034

unit_Day8_computeTask2 :: Assertion
unit_Day8_computeTask2 = computeTask2 parsedInput @?= 672
