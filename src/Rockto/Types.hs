module Rockto.Types where

import qualified System.Random as R (StdGen)

--------------------------------------------------------------------------------
-- | Representing Basic Map Elements
--------------------------------------------------------------------------------

data Tile
  = TBrick              -- Brick
  | TParcel             -- Parcel
  | TWall               -- Wall
  | TScaffold           -- Supports bricks and parcels above
  | TExit               -- Exit
  | TEmpty              -- Nothing there
  deriving (Eq, Ord, Enum, Show)

-- not sure any other necessary attributes?
newtype Map
  = Map { getMap :: [[Tile]]
        } deriving (Show)

--------------------------------------------------------------------------------
-- | Global State
--------------------------------------------------------------------------------

data GSt
  = GSt { _map               :: Map
        , _droppingPositions :: [(Int, Int)]
        , _seed              :: R.StdGen
        , _score             :: Int
        , _round             :: Int
        , _dead              :: Bool
        , _stable            :: Bool
        , _pos               :: (Int, Int)
        } deriving (Show)

data Direction
  = DUp
  | DDown
  | DLeft
  | DRight
  | DNull
  deriving (Eq, Enum, Show)
