{-# OPTIONS_GHC -Wno-missing-fields #-}
module TestTick
  ( spec_tick
  )
where

import Test.Hspec

import Rockto.Tick
import Rockto.Types

testMaps :: [Map]
testMaps = [
    Map [
      [TParcel, TScaffold],
      [TEmpty, TWall],
      [TScaffold, TWall]
    ]
  ]

testStates :: [GSt]
testStates = [
    GSt {
      _map = head testMaps,
      _droppingPositions = [],
      _target = 0,
      _round = 0,
      _dead = False,
      _stable = True,
      _finish = False,
      _pos = (0, 0)
    }
  ]



spec_tick :: SpecWith ()
spec_tick = describe "Rockto.Resource" $ do

  it "get positions where drop happens"
    $          getDropPositions (head testMaps) (1, 1)
    `shouldBe` [(0, 0)]

  it "tick with nothing happens"
    $          tick DNull (head testStates)
    `shouldBe` (head testStates)
