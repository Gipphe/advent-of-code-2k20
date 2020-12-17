module Day16Spec where

import Day16
import Test.Tasty.HUnit

unit_Day16_computeTask1 :: Assertion
unit_Day16_computeTask1 = computeTask1 parsedInput @?= 23044

unit_Day16_computeTask2 :: Assertion
unit_Day16_computeTask2 = computeTask2 parsedInput @?= 3765150732757
