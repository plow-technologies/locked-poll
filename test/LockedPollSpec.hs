module LockedPollSpec (tests) where

import LockedPoll

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" (
      \list -> sort (list :: [Int]) == sort (reverse list))
  , QC.testProperty "Fermat's little theorem" (
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0)
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" (
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer))
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" (
      [1, 2, 3] `compare` [1,2] @?= GT)

  -- the following test does not hold
  , testCase "List comparison (same length)" (
      [1, 2, 3] `compare` [1,2,2] @?= LT)
  ]