module TestUtils
  ( spec_utils
  )
where

import Test.Hspec

import Rockto.Types
import Rockto.Utils

testMaps :: [Map]
testMaps = [
    Map [
      [TParcel, TScaffold],
      [TEmpty, TWall],
      [TScaffold, TWall]
    ],
    Map [
      [TWall, TScaffold],
      [TEmpty, TWall],
      [TScaffold, TWall]
    ],
    Map [
      [TParcel, TScaffold],
      [TEmpty, TWall],
      [TScaffold, TWall]
    ]
  ]

spec_utils :: SpecWith ()
spec_utils = describe "Util" $ do

  it "get size of map along x-axis"
    $          getMapXSize demoMap
    `shouldBe` 16

  it "get size of map along y-axis"
    $          getMapYSize demoMap
    `shouldBe` 7

  it "get tile"
    $          getTile (testMaps!!0) (0, 0)
    `shouldBe` TParcel

  it "set tile"
    $          setTile (testMaps!!0) (0, 0) TWall
    `shouldBe` (testMaps!!1)

  it "step position up"
    $          stepPos DUp (0, 0)
    `shouldBe` (0, -1)

  it "step position down"
    $          stepPos DDown (0, 0)
    `shouldBe` (0, 1)

  it "step position left"
    $          stepPos DLeft (0, 0)
    `shouldBe` (-1, 0)

  it "step position right"
    $          stepPos DRight (0, 0)
    `shouldBe` (1, 0)

  it "step position null"
    $          stepPos DNull (0, 0)
    `shouldBe` (0, 0)

  it "set list"
    $          setList [0, 0, 0, 0] 1 9
    `shouldBe` [0, 9, 0, 0]
