module Rockto.Utils where

import Rockto.Types

import qualified System.Random as R (StdGen)

mkInitS :: R.StdGen -> GSt
mkInitS seed = GSt { _map = demoMap
                   , _seed  = seed
                   , _score = 0
                   , _round = 1
                   , _pos = (1, 1)
                   }

demoMap :: Map
demoMap = Map [[TParcel, TWall], [TScaffold, TEmpty]]
