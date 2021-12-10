module Rockto.Resource where

import Data.Char (isDigit)
import Data.List (elemIndex, intersect)
import Data.Maybe (fromJust)
import Rockto.Types
import System.IO (hFlush, stdout)

loadMap :: FilePath -> IO (Map, (Int, Int))
-- Load a Map and the starting position from a filepath
loadMap filename = do
  contents <- readFile filename
  let alineofMap = head (lines contents)
      width_of_Map = length alineofMap
      mapcontents = concat (lines contents)
      list_of_Tile = parseMapString mapcontents
      mymap = splitEvery width_of_Map list_of_Tile
      (x, y) = findPos 's' mapcontents width_of_Map
  return (Map mymap, (x, y))

findPos :: Char -> [Char] -> Int -> (Int, Int)
-- find a char's position in a concated char matrix with specific width 
findPos c str width = (rem, ans)
  where
    index = fromJust (elemIndex c str)
    ans = index `div` width
    rem = index `mod` width

loadGame :: Int -> IO Game
-- Load a specific game round, including the starting position of the game, 
-- the game map, and the number of targets
loadGame round = do
  mapInfo <- loadMap $ getFileName round
  let gameMap = fst mapInfo
      startPos = snd mapInfo
      targetNum = countTarget gameMap
  return
    Game
      { _gameRound = round
      , _gameMap = gameMap
      , _gameStartPos = startPos
      , _gameTargetNum = targetNum
      }

getFileName :: Int -> FilePath
-- get file name of specified game round
getFileName a = "resources/round_0" ++ show a ++ ".txt"

countTarget :: Map -> Int
-- count target number of a  map
countTarget gameMap = countParcel (concat (getMap gameMap)) TParcel

countParcel :: [Tile] -> Tile -> Int
-- count parcel number of a list of Tile
countParcel listTile t = length $ filter (== t) listTile

splitEvery :: Int -> [a] -> [[a]]
-- Split a list into smaller lists of a specific length
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = splitAt n list

charToTile :: Char -> Tile
-- Convert a Char to a Tile
charToTile ch
  | ch == 'o' = TBrick
  | ch == 'p' = TParcel
  | ch == 'x' = TWall
  | ch == '-' = TScaffold
  | ch == 'c' = TExit
  | ch == ' ' = TEmpty
  | ch == 's' = TEmpty
  | otherwise = TEmpty

parseMapString :: [Char] -> [Tile]
-- parse a [Char] to a [Tile], which is a Map
parseMapString = map charToTile
