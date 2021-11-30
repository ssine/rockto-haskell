module Rockto.Resource
  ( loadGame
  )
where

import System.IO (hFlush, stdout)

import Rockto.Types

loadMap :: FilePath -> IO Map
loadMap filename = return $ Map [[TBrick, TExit], [TWall, TEmpty]]

-- >>> parseMapString "xxo-"
-- [TWall,TWall,TParcel,TEmpty]

parseMapString :: String -> [Tile]
parseMapString str = [TEmpty]

-- Just a demo here
loadGame :: Int -> IO Round
loadGame round = do
  roundMap <- loadMap $ getFileName round
  return Round{ _roundMap=roundMap
                , _roundStartPosition=(1,1)
                , _roundTargetNum=4}

getFileName :: Int -> String
getFileName _ = "resources/round_01.txt"
