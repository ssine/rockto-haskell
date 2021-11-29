module Rockto.Resource
  ( loadGame
  )
where

import System.IO (hFlush, stdout)

import Rockto.Types
import Data.List (intersect)




loadMap :: FilePath -> IO Map
loadMap filename = do
        contents <- readFile filename
        let mapcontents = concat (map ((!!) (lines contents)) [2,4..(length (lines contents) - 1)])
            list_of_Tile = parseMapString mapcontents
            mymap =splitEvery 7 list_of_Tile
        return $ Map mymap

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

charToTile :: Char -> Tile
charToTile ch
      | ch =='o'= TBrick
      | ch =='p'= TParcel
      | ch =='x'= TWall
      | ch =='-'= TScaffold
      | ch =='c'= TExit
      | ch ==' '= TEmpty
      | ch =='s'= TEmpty

parseMapString :: [Char] -> [Tile]
parseMapString str = map charToTile str


loadPos :: FilePath -> IO (Int,Int)
loadPos filename = do
        contents <- readFile filename
        let xcontent = concat (map ((!!) (lines contents)) [0])
            ycontent = concat (map ((!!) (lines contents)) [1])
            x = readXY xcontent
            y = readXY ycontent
        return ((x,y))

readXY :: [Char] -> Int
readXY str=read(intersect str "0123456789")
-- >>> readXY "statrX = 3"
-- 3


loadTargetNum :: Map -> IO Int
loadTargetNum gameMap = do
  let targetNum = countParcel (concat (getMap gameMap) ) TParcel
  return targetNum


countParcel :: [Tile] -> Tile -> Int
countParcel listTile t = length $ filter (== t ) listTile

-- >>>countParcel [TWall, TScaffold, TBrick, TParcel] TParcel
-- 1
 
loadGame :: Int -> IO Round
loadGame round = do
  roundMap <- loadMap $ getFileName round
  startPos <- loadPos $ getFileName round
  targerNum <- loadTargetNum roundMap
  return Round{ _roundMap=roundMap
                , _roundStartPosition=startPos
                , _roundTargetNum=targerNum}

getFileName :: Int -> FilePath
getFileName a = "resources/round_0"++show a++".txt"

