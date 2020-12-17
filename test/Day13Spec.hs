module Day13Spec where

import Day13
import Test.Tasty.HUnit

unit_Day13_computeTask1 :: Assertion
unit_Day13_computeTask1 = computeTask1 parsedInput @?= 2298

unit_Day13_computeTask2 :: Assertion
unit_Day13_computeTask2 = computeTask2 parsedInput @?= 783685719679632
