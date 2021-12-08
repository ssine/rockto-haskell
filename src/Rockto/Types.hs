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
  | TPlayer             -- No place but UI can use it
  deriving (Eq, Ord, Enum, Show)

newtype Map
  = Map { getMap :: [[Tile]]
        } deriving (Show)

--------------------------------------------------------------------------------
-- | Global State
--------------------------------------------------------------------------------

data GSt
  = GSt { _map               :: Map
        , _droppingPositions :: [(Int, Int)]    -- used by "animation"
        , _seed              :: R.StdGen        -- global random seed
        , _target            :: Int             -- number of remained parcels
        , _round             :: Int             -- round of current game
        , _dead              :: Bool            -- game status: dead
        , _stable            :: Bool            -- used by "animation"
        , _finish            :: Bool            -- game status: pass current round
        , _pos               :: (Int, Int)      -- player position
        } deriving (Show)

--------------------------------------------------------------------------------
-- | Direction of Movement
--------------------------------------------------------------------------------

data Direction
  = DUp
  | DDown
  | DLeft
  | DRight
  | DNull
  deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
-- | Game Setting
--------------------------------------------------------------------------------

data Game =
  Game { _gameRound     :: Int            -- round of the game
       , _gameMap       :: Map
       , _gameStartPos  :: (Int, Int)     -- start position of the player
       , _gameTargetNum :: Int            -- number of parcels to collect
       }
