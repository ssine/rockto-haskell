{-# LANGUAGE OverloadedStrings #-}

module Main where

import UI (drawUI)

import Rockto.Types
import Rockto.Utils (mkInitS)
import Rockto.Movement (move)

import qualified System.Random as R (newStdGen)
import qualified Graphics.Vty as V

import Brick
import Brick.Main
  ( App(..), defaultMain
  , neverShowCursor
  )
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)

import Control.Monad (void, forever)


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.magenta)
    , ("keyword2",      V.white `on` V.blue)
    ]

handleEvent :: GSt -> BrickEvent () e -> EventM () (Next GSt)
handleEvent st (VtyEvent (V.EvKey key [])) =
    case key of
        V.KEsc          -> halt st
        V.KChar 'q'     -> halt st
        V.KUp           -> continue $ move DUp st
        V.KDown           -> continue $ move DDown st
        V.KLeft           -> continue $ move DLeft st
        V.KRight           -> continue $ move DRight st
        _ -> continue st
handleEvent st _ = continue st

app :: App GSt e ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    n <- R.newStdGen
    void $ defaultMain app $ mkInitS n 
