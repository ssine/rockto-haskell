module TestResource
  ( spec_resource
  )
where

import Test.Hspec

import Rockto.Resource

spec_resource :: SpecWith ()
spec_resource = describe "Rockto.Resource" $ do

  it "get file name of specified game round"
    $          getFileName 2
    `shouldBe` "resources/round_02.txt"
