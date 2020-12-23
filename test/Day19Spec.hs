module Day19Spec where

import Day19
import Test.Tasty.HUnit

unit_Day19_computeTask1 :: Assertion
unit_Day19_computeTask1 = computeTask1 parsedInput @?= 279

unit_Day19_computeTask2 :: Assertion
unit_Day19_computeTask2 = computeTask2 parsedInput @?= 384
