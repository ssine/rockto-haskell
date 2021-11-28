module Rockto.FileIO
  ( loadMap
  ) where

import System.IO (hFlush, stdout)

import Rockto.Types

loadMap :: FilePath -> IO Map
loadMap = \_ -> return $ Map [[TBrick, TExit], [TWall, TEmpty]]
