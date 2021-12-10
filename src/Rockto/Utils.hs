module Rockto.Utils where

import Rockto.Types

import qualified System.Random as R (StdGen)

initSt :: Game -> R.StdGen -> GSt
initSt game seed = GSt { _seed  = seed
                       , _round = _gameRound game
                       , _map = _gameMap game
                       , _pos = _gameStartPos game
                       , _target = _gameTargetNum game
                       , _dead = False
                       , _finish = False
                       , _stable = True
                       , _droppingPositions = []
                       }

demoMap :: Map
demoMap = Map [[TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall],
  [TWall, TScaffold, TScaffold, TScaffold, TRock, TScaffold, TScaffold, TParcel, TScaffold, TScaffold, TParcel, TScaffold, TScaffold, TScaffold, TScaffold, TWall],
  [TWall, TWall, TWall, TWall, TRock, TScaffold, TScaffold, TParcel, TScaffold, TScaffold, TRock, TScaffold, TScaffold, TScaffold, TScaffold, TWall],
  [TWall, TScaffold, TEmpty, TScaffold, TScaffold, TScaffold, TScaffold, TScaffold, TScaffold, TScaffold, TParcel, TScaffold, TScaffold, TExit, TScaffold, TWall],
  [TWall, TScaffold, TScaffold, TScaffold, TEmpty, TScaffold, TScaffold, TEmpty, TScaffold, TScaffold, TScaffold, TScaffold, TWall, TWall, TWall, TWall],
  [TWall, TScaffold, TScaffold, TScaffold, TEmpty, TScaffold, TScaffold, TRock, TScaffold, TScaffold, TEmpty, TScaffold, TScaffold, TScaffold, TScaffold, TWall],
  [TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall, TWall]]

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
