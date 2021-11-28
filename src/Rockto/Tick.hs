{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Rockto.Tick where

import Rockto.Types
import Rockto.Utils

tick :: Direction -> GSt -> GSt

-- boundary condition
tick DNull state@GSt{_stable = True} = state

-- steady player, tick time (check for unsupported brick & parcels, player hit detection)
tick DNull state
  | null dropPositions = state {_stable = True}
  | otherwise = foldl handleDroppable state dropPositions
  where
    mp = _map state
    lenX = getMapXSize mp
    lenY = getMapYSize mp
    dropPositions = [(x, y) |
      x <- [0..10],
      y <- [0..10],
      getTile mp (x, y) `elem` [TParcel, TBrick],
      getTile mp (stepPos (x, y) DDown) == TEmpty ]
    handleDroppable state' position
      | newPosition == _pos state' = newState {_dead = True}
      | otherwise = newState
      where
        mp' = _map state'
        newPosition = stepPos position DDown
        tile = getTile mp' position
        newState = state' {_map = setTile (setTile mp' position TEmpty) newPosition tile}

-- state not stable, ignore input
tick direction state@GSt{_stable = False} = state

-- update player only
tick direction state
  | nextTile `elem` [TBrick, TWall] = state
  | nextTile `elem` [TScaffold, TEmpty] = state {
      _map = setTile (_map state) nextPos TEmpty,
      _stable = False,
      _pos = nextPos
    }
  | nextTile == TParcel = state {
      _map = setTile (_map state) nextPos TEmpty,
      _score = _score state + 1,
      _stable = False,
      _pos = nextPos
    }
  | nextTile == TExit = state  -- TODO: use next map
  where
    nextPos = stepPos (_pos state) direction
    nextTile = getTile (_map state) nextPos
