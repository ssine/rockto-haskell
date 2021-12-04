module Rockto.Resource
  ( loadGame
  )
where

import System.IO (hFlush, stdout)

import Rockto.Types
import Data.Char (isDigit)
import Data.List (intersect, elemIndex)
import Data.Maybe (fromJust)


loadMap :: FilePath -> IO (Map,(Int,Int))
loadMap filename = do
        contents <- readFile filename
        let alineofMap = (lines contents)!!0 
            width_of_Map =length alineofMap
            mapcontents = concat (lines contents)
            list_of_Tile = parseMapString mapcontents
            mymap =splitEvery width_of_Map list_of_Tile
            (x,y)=findPos 's' mapcontents width_of_Map
        return $ (Map mymap,(x,y))


findPos :: Char-> [Char] -> Int ->(Int, Int)
findPos c str width = (rem, ans) 
  where index = fromJust (elemIndex c str)
        ans   = index `div` width
        rem   = index `mod` width
 
loadGame :: Int -> IO Round
loadGame round = do
  mapInfo <- loadMap $ getFileName round
  let roundMap = fst mapInfo
      startPos = snd mapInfo
  targetNum <- countTarget roundMap
  return Round{ _roundMap=roundMap
                , _roundStartPosition=startPos
                , _roundTargetNum=targetNum}



getFileName :: Int -> FilePath
getFileName a = "resources/round_0"++show a++".txt"

countTarget :: Map -> IO Int
countTarget gameMap = do
  let targetNum = countParcel (concat (getMap gameMap) ) TParcel
  return targetNum


countParcel :: [Tile] -> Tile -> Int
countParcel listTile t = length $ filter (== t ) listTile


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


