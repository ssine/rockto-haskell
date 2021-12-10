module Main where

import Test.Hspec (hspec)

import TestResource (spec_resource)
import TestTick (spec_tick)
import TestUtils (spec_utils)


main :: IO ()
main = hspec $ do
  spec_utils
  spec_resource
  spec_tick
