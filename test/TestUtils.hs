module TestUtils
  ( spec_utils
  )
where

import Test.Hspec

import Rockto.Utils

spec_utils :: SpecWith ()
spec_utils = describe "Util" $ do

  it "get size of map along x-axis"
    $          getMapXSize demoMap
    `shouldBe` 16

  it "get size of map along y-axis"
    $          getMapYSize demoMap
    `shouldBe` 7
