module Day11Spec where

import Day11
import Test.Tasty.HUnit

unit_Day11_computeTask1 :: Assertion
unit_Day11_computeTask1 = computeTask1 parsedInput @?= 2222

unit_Day11_computeTask2 :: Assertion
unit_Day11_computeTask2 = computeTask2 parsedInput @?= 2032
