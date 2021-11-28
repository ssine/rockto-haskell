module Rockto.Utils where

import Rockto.Types

import qualified System.Random as R (StdGen)

mkInitS :: R.StdGen -> GSt
mkInitS seed = GSt { _map = demoMap
                   , _droppingPositions = []
                   , _seed  = seed
                   , _score = 0
                   , _round = 1
                   , _dead = False
                   , _stable = True
                   , _pos = (1, 1)
                   }

demoMap :: Map
demoMap = Map [ [TParcel, TWall]
              , [TScaffold, TEmpty]
              , [TScaffold, TWall]
              ]

getMapYSize :: Map -> Int
getMapYSize = length . getMap

getMapXSize :: Map -> Int
getMapXSize = length . head . getMap

{- Corrdinate System:
   ┌───►
   │   x
   ▼y
-}
getTile :: Map -> (Int, Int) -> Tile
getTile mp (x, y)
  | 0 <= x && x < lenX && 0 <= y && y < lenY = getMap mp !! y !! x
  | otherwise = TWall
  where
    lenX = getMapXSize mp
    lenY = getMapYSize mp

stepPos :: Direction -> (Int, Int) -> (Int, Int)
stepPos DUp    (x, y) = (x, y - 1)
stepPos DDown  (x, y) = (x, y + 1)
stepPos DLeft  (x, y) = (x - 1, y)
stepPos DRight (x, y) = (x + 1, y)
stepPos DNull  (x, y) = (x, y)

setList :: [a] -> Int -> a -> [a]
setList list index element = x ++ element : ys
  where (x, _:ys) = splitAt index list


setTile :: Map -> (Int, Int) -> Tile -> Map
setTile mp (x, y) tile
  | 0 <= x && x < lenX && 0 <= y && y < lenY = Map $ setList list y $ setList (list !! y) x tile
  | otherwise = mp
  where
    lenX = getMapXSize mp
    lenY = getMapYSize mp
    list = getMap mp
