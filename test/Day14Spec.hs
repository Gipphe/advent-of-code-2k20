module Day14Spec where

import Day14
import Test.Tasty.HUnit

unit_Day14_computeTask1 :: Assertion
unit_Day14_computeTask1 = computeTask1 parsedInput @?= 10885823581193

unit_Day14_computeTask2 :: Assertion
unit_Day14_computeTask2 = computeTask2 parsedInput @?= 3816594901962
