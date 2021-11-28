module Rockto.Movement where

import Rockto.Types

move :: Direction -> GSt -> GSt
move d st = st { _map = nmap
               , _score = nscore
               , _pos = npos
               , _round = nround}
  where
    nmap = _map st
    nscore = _score st
    npos
      | d == DUp =
        if oy < ySize - 1
          then (ox, oy + 1)
          else originPos
      | d == DDown =
        if oy > 0
          then (ox, oy - 1)
          else originPos
      | d == DLeft = (ox - 1, oy)
      | d == DRight = (ox + 1, oy)
      | otherwise = originPos
      where
        originPos = _pos st
        (ox, oy) =  originPos
        xSize = getMapXSize $ _map st
        ySize = getMapYSize $ _map st
    nround = _round st


getMapYSize :: Map -> Int
getMapYSize = length . getMap

getMapXSize :: Map -> Int
getMapXSize = length . head . getMap
