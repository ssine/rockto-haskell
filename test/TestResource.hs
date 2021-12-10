module TestResource
  ( spec_resource
  )
where

import Test.Hspec
import Rockto.Resource
import Rockto.Types
import Rockto.Utils (demoMap)

spec_resource :: SpecWith ()
spec_resource = describe "Rockto.Resource" $ do

  it "get file name of specified game round"
    $          getFileName 1
    `shouldBe` "resources/round_01.txt"   

  it "parse a string to a map"
    $          parseMapString "opx-c s"
    `shouldBe`   [TBrick,TParcel,TWall,TScaffold,TExit,TEmpty,TEmpty]

  it "count target number of a  map"
    $          countTarget demoMap
    `shouldBe`   4

  it "count parcel number of a list of Tile"
    $          countParcel [TBrick,TParcel,TWall,TScaffold,TExit,TEmpty,TEmpty] TParcel
    `shouldBe`   1

  it "Split a list into smaller lists of a specific length "
    $          splitEvery 4 [1,2,3,4,5,6,7,8]
    `shouldBe`   [[1,2,3,4],[5,6,7,8]]

  it "Convert a Char to a Tile "
    $          charToTile 'o'
    `shouldBe`   TBrick

  it "Convert a Char to a Tile "
    $          charToTile 'p'
    `shouldBe`   TParcel

  it "Convert a Char to a Tile "
    $          charToTile 'x'
    `shouldBe`   TWall

  it "Convert a Char to a Tile "
    $          charToTile '-'
    `shouldBe`   TScaffold

  it "Convert a Char to a Tile "
    $          charToTile 'c'
    `shouldBe`   TExit

  it "Convert a Char to a Tile "
    $          charToTile ' '
    `shouldBe`   TEmpty

  it "Convert a Char to a Tile "
    $          charToTile 's'
    `shouldBe`   TEmpty

  it "find a char's position in a concated char matrix with specific width  "
    $          findPos 'b' "abcd" 2
    `shouldBe`   (1,0)



