module Rockto.Tick where

import Rockto.Types
import Rockto.Utils


tick :: Direction -> GSt -> GSt

-- boundary condition
tick DNull state@GSt{_stable = True} = state

-- steady player, tick time (check for unsupported brick & parcels, player hit detection)
tick DNull state
  | null dropPositions && null (_droppingPositions state) = state {_stable = True}
  | otherwise =
      let
        stAfterDrop = foldl handleDroppable state dropPositions
        stAfterHit = foldl handleDropping stAfterDrop (_droppingPositions state)
      in
        stAfterHit {_droppingPositions = map (stepPos DDown) dropPositions}
  where
    dropPositions = getDropPositions (_map state) (_pos state)
    handleDroppable state' position = newState
      where
        mp' = _map state'
        newPosition = stepPos DDown position
        tile = getTile mp' position
        newState = state' {_map = setTile (setTile mp' position TEmpty) newPosition tile}
    handleDropping state' position
      | newPosition == _pos state' = newState {_dead = True}
      | otherwise = state'
      where
        mp' = _map state'
        newPosition = stepPos DDown position
        tile = getTile mp' position
        newState = state' {_map = setTile (setTile mp' position TEmpty) newPosition tile}

-- state not stable, ignore input
tick direction state@GSt{_stable = False} = state

-- update player only
tick direction state
  | nextTile == TWall = state
  | direction `elem` [DLeft, DRight] && nextTile == TBrick && nextTile2 == TEmpty =
      let newMap = setTile (setTile (_map state) nextPos TEmpty) nextPos2 TBrick
      in
        state {
          _map = newMap,
          _stable = null $ getDropPositions newMap nextPos,
          _pos = nextPos
        }
  | nextTile == TBrick = state
  | nextTile `elem` [TScaffold, TEmpty] =
      let newMap = setTile (_map state) nextPos TEmpty
      in
        state {
          _map = newMap,
          _stable = null $ getDropPositions newMap nextPos,
          _pos = nextPos
        }
  | nextTile == TParcel =
      let newMap = setTile (_map state) nextPos TEmpty
      in
        state {
          _map = newMap,
          _score = _score state + 1,
          _stable = null $ getDropPositions newMap nextPos,
          _pos = nextPos
        }
  | nextTile == TExit = state  -- TODO: use next map
  | otherwise = state
  where
    nextPos = stepPos direction (_pos state)
    nextTile = getTile (_map state) nextPos
    nextPos2 = stepPos direction nextPos
    nextTile2 = getTile (_map state) nextPos2


getDropPositions :: Map -> (Int, Int) -> [(Int, Int)]
getDropPositions mp playerPosition =
  let
    lenX = getMapXSize mp
    lenY = getMapYSize mp
  in
    [(x, y) |
      x <- [0..lenX],
      y <- [0..lenY],
      getTile mp (x, y) `elem` [TParcel, TBrick],
      let downPosition = stepPos DDown (x, y)
      in getTile mp downPosition == TEmpty && downPosition /= playerPosition ]
