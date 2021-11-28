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
      | d == DUp = (ox, oy + 1)
      | d == DDown = (ox, oy - 1)
      | d == DLeft = (ox - 1, oy)
      | d == DRight = (ox + 1, oy)
      | otherwise = (ox, oy)
      where (ox, oy) =  _pos st
    nround = _round st
