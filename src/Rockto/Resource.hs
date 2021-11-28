module Rockto.Resource
  ( loadMap
  ) where

import System.IO (hFlush, stdout)

import Rockto.Types

loadMap :: FilePath -> IO Map
loadMap filename = return $ Map [[TBrick, TExit], [TWall, TEmpty]]

-- >>> parseMapString "xxo-"
-- [TWall,TWall,TParcel,TEmpty]

parseMapString :: String -> [Tile]
parseMapString str = [TEmpty]
